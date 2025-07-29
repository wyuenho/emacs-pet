;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-merge-eglot-initialization-options"
  (it "should deeply merge 2 plists"
    (expect
     (pet-merge-eglot-initialization-options
      '(:a (:b [1 2] :c 0 :d "hello" :f :json-null))
      '(:a (:b [3 4] :c 9 :e "world" :g :json-false)))
     :to-equal
     '(:a (:b [1 2 3 4] :c 9 :d "hello" :f :json-null :e "world" :g :json-false)))))

