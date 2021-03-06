
;;;; sessions.lisp

(in-package #:eshop)

(defvar *session-secret* nil)

(defclass session (eshop.odm:persistent-object)
  ((session-string :reader session-string
                   :documentation "The session string encodes enough
data to safely retrieve this session.  It is sent to the browser as a
cookie value or as a GET parameter.")
   (user-agent :initform (hunchentoot:user-agent hunchentoot:*request*)
               :serializable t
               :reader session-user-agent
               :documentation "The incoming 'User-Agent' header that
was sent when this session was created.")
   (remote-addr :initform (hunchentoot:real-remote-addr hunchentoot:*request*)
                :serializable t
                :reader session-remote-addr
                :documentation "The remote IP address of the client
when this session was started as returned by REAL-REMOTE-ADDR.")
   (user :serializable t
         :accessor session-user
         :initform nil
         :initarg :user)
   (cart :initarg :cart
         :serializable t
         :type string)
   (nc-user :initarg :nc-user
            :serializable t
            :type string))
  (:metaclass eshop.odm:persistent-class))

(defmethod key ((session session))
  (eshop.odm:serializable-object-key session))

(defun session-value* (slot)
  (slot-value hunchentoot:*session* slot))

(defun encode-session-string (id user-agent remote-addr)
  "Creates a uniquely encoded session string based on the values ID,
USER-AGENT and REMOTE-ADDR"
  (unless (boundp '*session-secret*)
    (log:warn "Session secret is unbound.  Using Lisp's RANDOM function to initialize it.")
    (reset-session-secret))
  ;; *SESSION-SECRET* is used twice due to known theoretical
  ;; vulnerabilities of MD5 encoding
  (md5-hex (concatenate 'string
            *session-secret*
            (md5-hex (format nil "~A~D~A~A"
                             *session-secret*
                             id
                             user-agent
                             remote-addr)))))

(defun stringify-session (session)
  "Creates a string representing the SESSION object SESSION. See
ENCODE-SESSION-STRING."
  (encode-session-string (key session)
                         (session-user-agent session)
                         (session-remote-addr session)))

(defmethod initialize-instance ((session session) &rest init-args)
  "Set SESSION-STRING slot after the session has been initialized."
  (declare (ignore init-args))
  (call-next-method)
  (setf (slot-value session 'session-string) (stringify-session session)))

(defmethod hunchentoot:session-cookie-value ((session session))
  "Returns a string unique for this SESSION to restore and verify the session object later."
  (when session
    (format nil
            "~D:~A"
            (key session)
            (session-string session))))

(defun session-verify (session-identifier)
  "Restore the session object and verify that the client is the one who started it.
  Return the session object or nil. Or create a new session instead of returning nil."
  (destructuring-bind (id-string session-string)
      (split ":" session-identifier :limit 2)
    (eshop.odm:with-transaction
      (let* ((id (parse-integer id-string))
             (session (eshop.odm:getobj 'session id))
             (user-agent (hunchentoot:user-agent hunchentoot:*request*))
             (remote-addr (hunchentoot:remote-addr hunchentoot:*request*))
             (expected-session-string (when session
                                        (encode-session-string
                                         id
                                         user-agent
                                         (hunchentoot:real-remote-addr
                                          hunchentoot:*request*))))
             (session-session-string (when session
                                       (session-string session))))
        (cond
          ((and session
                (string= session-string
                         session-session-string)
                (string= session-string
                         expected-session-string))
           ;; the session key presented by the client is valid
           (merge-session session)
           session)
          (session
           ;; the session ID pointed to an existing session, but the
           ;; session string did not match the expected session string
           (log:warn "Fake session identifier '~A' (User-Agent: '~A', IP: '~A')"
                     session-identifier user-agent remote-addr)
           nil)
          (t
           (log:info "No session for session identifier '~A' (User-Agent: '~A', IP: '~A')"
                     session-identifier user-agent remote-addr)
           nil))))))

(defun new-session (&key persistent)
  (setf (hunchentoot:session hunchentoot:*request*) nil)
  (setf hunchentoot:*session* nil)
  (start-session :persistent persistent))

(defun bot-request (&optional (user-agent (hunchentoot:user-agent)))
  (let ((bot-p (some #'(lambda (bot)
                         (search bot user-agent))
                     (config.get-option :other-options :bot-useragents))))
    (when bot-p
      (metric:count "bots"))
    bot-p))

(defun start-session (&key persistent)
  "Returns the current SESSION object. If there is no current session,
creates one and updates the corresponding data structures. In this
case the function will also send a session cookie to the browser."
  (unless (bot-request)
    (let ((session (hunchentoot:session hunchentoot:*request*)))
      (when session
        (return-from start-session session))
      (setf session (make-instance 'session)
            (hunchentoot:session hunchentoot:*request*) session)
      (hunchentoot:set-cookie (hunchentoot:session-cookie-name hunchentoot:*acceptor*)
                              :value (hunchentoot:session-cookie-value session)
                              :expires (if persistent
                                           (+ (get-universal-time)
                                              (* 60 60 24 30))
                                           nil)
                              :path "/")
      (hunchentoot:session-created hunchentoot:*acceptor* session)
      (setq hunchentoot:*session* session))))

(defun merge-session (session)
  (dolist (cookie '("cart" "nc-user"))
    (let ((slot (anything-to-symbol cookie))
          (value (hunchentoot:cookie-in cookie)))
      (if (and (slot-boundp session slot)
               (not value))
          (hunchentoot:set-cookie cookie :value (slot-value session slot))
          (when value
            (setf (slot-value session slot) value))))))

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

(defun reset-session-secret ()
  "Sets *SESSION-SECRET* to a new random value. All old sessions will
cease to be valid."
  (setq *session-secret* (create-random-string 10 36)))
