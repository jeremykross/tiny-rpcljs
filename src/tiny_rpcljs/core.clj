(ns tiny-rpcljs.core
  (:require [clojure.string :as string]))

(defn form->rpc-call
  [target form]
  (let [function (name (first form))
        args (into [] (rest form))
        mutating? (clojure.string/ends-with? function "!")
        method 'ajax.core/GET]
    `(let [out-ch# (cljs.core.async/chan)]
      (~method ~(str "http://" target "/rpc/" (name function))
             {:format :json
              :response-format :json
              :keywords? true
              :params {:args ~args}
              :error-handler (fn [err#]
                               (cljs.core.async.macros/go
                                 (cljs.core.async/>! out-ch# :FAILED)))
              :handler (fn [res#]
                         (cljs.core.async.macros/go
                           (cljs.core.async/>! out-ch# (:returned res#))))})
       (cljs.core.async/<! out-ch#))))

(defn form->rpc-defn 
  [server form]
  (let [form (rest form)
        function (first form)
        mutating? (string/ends-with? function "!")
        args (second form)
        body (subvec (into [] form) 2)
        method (if mutating? '.post '.get)]
    `(do 
       (defn ~function
         ~args
         ~@body)
       (~method ~server
                (fn [req# res#]
                  (let [body# (cljs.core/js->clj (js/JSON.parse (.-body req#))
                                       :keywords? true)]
                    (let [result# (apply ~function (:args body#))]
                      (.send res# (cljs.core/clj->js result#)))))))))


(defmacro rpc-serve
  [port & body]
  (let [server-name (gensym)]
    `(let [~server-name (express)]
       ~@(map (partial form->rpc-defn server-name) body)
       ~server-name)))


(defmacro rpc
  [remote & body]
  `(cljs.core.async.macros/go
     ~@(map (partial form->rpc-call remote) body)))
