;;;; package.lisp

(defpackage :conveniences
  (:use :common-lisp)
  (:export :add-to-hash
           :explain
           :for-each
           :for-each-list
           :for-each-hash))

(defpackage :iexportplaylists
  (:use :common-lisp
        :conveniences)
  (:export :main
           :*ignored-playlists*))

