
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                       ;;
;;   conveniences.lisp                                   ;;
;;                                                       ;;
;;                Author: Tony Fischetti                 ;;
;;                        tony.fischetti@gmail.com       ;;
;;                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;  Convenience functions for iExplorePlaylists  ;

(in-package :conveniences)

(defmacro add-to-hash (the-hash key value)
  `(setf (gethash ,key ,the-hash) ,value))

(defmacro explain (text &rest body)
  `(progn (format t "~A~%" ,text) ,@body))

(defun gen-size (a-thing)
  (etypecase a-thing
    (vector        (length a-thing))
    (list          (length a-thing))
    (hash-table    (hash-table-count a-thing))))

(defmacro for-each ((index item a-thing &key (progress? nil) (offset "")) &body body)
  (cond
    (progress?
      (let ((len (gensym)))
        `(let ((,len (gen-size ,a-thing)))
           (for-each (,index ,item ,a-thing)
             (when ,progress?
               (format t "~AOn ~A of ~A........... ~,2F%~%"
                       ,offset (+ 1 ,index) ,len (* 100 (/ (+ 1 ,index) ,len))))
             ,@body))))
    (t
      `(etypecase ,a-thing
         (list          (for-each-list   (,index ,item ,a-thing) ,@body))
         (vector        (for-each-vector (,index ,item ,a-thing) ,@body))
         (hash-table    (for-each-hash   (,index ,item ,a-thing) ,@body))))))

(defmacro for-each-list ((index item a-list) &body body)
  `(let ((,index -1)) (dolist (,item ,a-list) (incf ,index) ,@body)))

(defmacro for-each-vector ((index item a-vector) &body body)
  `(let ((,index -1)) (loop for ,item across ,a-vector
                            do (progn (incf ,index) ,@body))))

(defmacro for-each-hash ((index item a-hash) &body body)
  `(let ((,index -1)) (loop for ,item being the hash-keys of ,a-hash
                            do (progn (incf ,index) ,@body))))

