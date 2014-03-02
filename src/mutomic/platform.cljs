(ns mutomic.platform
  "Host platform dependent code."
  (:require [cljs.reader :as edn]
            [clojure.string :as string]))

(defn starts-with [string start]
  (.startsWith string start))

(defn to-upper-case [string]
  (.toUpperCase string))

(defn illegal-argument [msg]
  (throw (js/Error. (str "Illegal argument: " msg))))

(def munge-char-map
  {\- "_"
   \: "_COLON_"
   \+ "_PLUS_"
   \> "_GT_"
   \< "_LT_"
   \= "_EQ_"
   \~ "_TILDE_"
   \! "_BANG_"
   \@ "_CIRCA_"
   \# "_SHARP_"
   \' "_SINGLEQUOTE_"
   \" "_DOUBLEQUOTE_"
   \% "_PERCENT_"
   \^ "_CARET_"
   \& "_AMPERSAND_"
   \* "_STAR_"
   \| "_BAR_"
   \{ "_LBRACE_"
   \} "_RBRACE_"
   \[ "_LBRACK_"
   \] "_RBRACK_"
   \/ "_SLASH_"
   \\ "_BSLASH_"
   \? "_QMARK_"})

(defn clj-munge [string]
  "See clojure.lang.Compiler/munge"
  (apply str (map #(munge-char-map % %) string)))

(def js-reserved
  #{"abstract" "boolean" "break" "byte" "case"
    "catch" "char" "class" "const" "continue"
    "debugger" "default" "delete" "do" "double"
    "else" "enum" "export" "extends" "final"
    "finally" "float" "for" "function" "goto" "if"
    "implements" "import" "in" "instanceof" "int"
    "interface" "let" "long" "native" "new"
    "package" "private" "protected" "public"
    "return" "short" "static" "super" "switch"
    "synchronized" "this" "throw" "throws"
    "transient" "try" "typeof" "var" "void"
    "volatile" "while" "with" "yield" "methods"
    "null"})

(defn munge
  "See cljs.compiler/munge"
  ([s] (munge s js-reserved))
  ([s reserved]
   (let [ss (string/replace (str s) #"\\/(.)" ".$1")
         ss (apply str (map #(if (reserved %) (str % "$") %)
                            (string/split ss #"(?=\.)")))
         ms (clj-munge ss)]
     (if (symbol? s)
       (symbol ms)
       ms))))

(defn resolve [sym]
  (let [ns (namespace sym)
        name (name sym)]
    (apply aget js/window (conj (string/split (or ns "cljs.core") #"\.") (munge name)))))

(defn read-edn [readers f]
  (throw (js/Error. "Not implemented!")))
