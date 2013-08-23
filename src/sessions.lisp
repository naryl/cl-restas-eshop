
;;;; routes.lisp

(in-package #:eshop)

(defparameter *serializable-session-vars* (list "cart"))

(defclass session ()
  ((user-agent :initform (hunchentoot:user-agent hunchentoot:*request*)
               :reader session-user-agent
               :documentation "The incoming 'User-Agent' header that
was sent when this session was created.")
   (remote-addr :initform (hunchentoot:real-remote-addr hunchentoot:*request*)
                :reader session-remote-addr
                :documentation "The remote IP address of the client
when this session was started as returned by REAL-REMOTE-ADDR.")
   (session-start :initform (get-universal-time)
                  :reader session-start
                  :documentation "The time this session was started.")
   (last-click :initform (get-universal-time)
               :reader session-last-click
               :documentation "The last time this session was used.")
   (max-time :initarg :max-time
             :initform hunchentoot:*session-max-time*
             :accessor session-max-time
             :type fixnum
             :documentation "The time \(in seconds) after which this
session expires if it's not used.")))

(defmethod hunchentoot:session-cookie-value ((session session))
  "Returns a string unique for this SESSION to restore and verify the session object later."
  "asdf")

(defun session-verify (session-identifier)
  "Restore the session object and verify that the client is the one who started it.
  Return the session object or nil. Or create a new session instead of returning nil."
  nil)

(defun init-session ()
  "Synchronize session and cookie variables. Take cookie if differ."
  (make-session)
  #+(or)
  (dolist (session-var *serializable-session-vars*)
    (aif (hunchentoot:cookie-in session-var)
         (setf (hunchentoot:session-value session-var) it)
         (awhen (hunchentoot:session-value session-var)
           (hunchentoot:set-cookie session-var :value it)))))

(defun make-session ()
  (aif hunchentoot:*session*
       it
       (let ((session (make-instance 'session)))
         (hunchentoot:set-cookie (hunchentoot:session-cookie-name hunchentoot:*acceptor*)
                                 :value (hunchentoot:session-cookie-value session))
         session)))

(defmethod hunchentoot:session-verify ((request hunchentoot:request))
 (let ((session-identifier (or (when-let (session-cookie (hunchentoot:cookie-in (hunchentoot:session-cookie-name hunchentoot:*acceptor*) request))
                                  (hunchentoot:url-decode session-cookie))
                                (hunchentoot:get-parameter (hunchentoot:session-cookie-name hunchentoot:*acceptor*) request))))
    (unless (and session-identifier
                 (stringp session-identifier)
                 (plusp (length session-identifier)))
      (return-from hunchentoot:session-verify nil))
    (let ((hunchentoot:*request* request))
      (session-verify session-identifier))))
