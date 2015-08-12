;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla REPL
;;; 
;;; Welcome to gorilla :-)
;;; 
;;; Shift + enter evaluates code. Hit ctrl+g twice in quick succession or click the menu icon (upper-right corner) for more commands ...
;;; 
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
(ns mljalfa.notebook.two
  (:require [gorilla-plot.core :refer :all]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(defn fdir [fname] (str "resources/relevant/" fname ".edn"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;mljalfa.notebook.two/fdir</span>","value":"#'mljalfa.notebook.two/fdir"}
;; <=

;; @@
(defn open [fname] (->> (fdir fname) slurp read-string))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;mljalfa.notebook.two/open</span>","value":"#'mljalfa.notebook.two/open"}
;; <=

;; @@

;; @@
