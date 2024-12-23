(defstruct uri-struct
  scheme
  userInfo
  host
  port
  path
  query
  fragment
  )


(defun urilib-scheme (uri) 
  (uri-struct-scheme uri)
  )

(defun urilib-userinfo (uri) 
  (uri-struct-userinfo uri)
  )

(defun urilib-host (uri) 
  (uri-struct-host uri)
  )

(defun urilib-port (uri) 
  (uri-struct-port uri)
  )

(defun urilib-path (uri) 
  (uri-struct-path  uri)
  )

(defun urilib-query (uri) 
  (uri-struct-query uri)
  )

(defun urilib-fragment (uri) 
  (uri-struct-fragment uri)
  )

;;this function must disappear!
(defun create-struct-temp (scheme userInfo host port path query fragment) 
  (make-uri-struct :scheme scheme :userInfo userInfo
                    :host host :port port :path path :query query :fragment fragment)
  )



(defun urilib-display (urilib-struct &optional (stream T)) 
  (if (not (typep urilib-struct 'URI-STRUCT))
      (error "incompatible type")
      (progn 
        (format stream "Scheme: ~13T ~S ~%" (urilib-scheme urilib-struct) )
        (format stream "UserInfo: ~13T ~S ~%" (urilib-userInfo urilib-struct) )
        (format stream "Host: ~13T ~S ~%" (urilib-host urilib-struct) )
        (format stream "Port: ~13T ~S ~%" (urilib-port urilib-struct) )
        (format stream "Path: ~13T ~S ~%" (urilib-path urilib-struct) )
        (format stream "Query: ~13T ~S ~%" (urilib-query urilib-struct) )
        (format stream "Fragment: ~13T ~S" (urilib-fragment urilib-struct) )
        (if (not (equal stream T)) (close stream) t)
        )
   
      )
  )

(defun urilib-parse (uri)
  (
   if (stringp uri) 
   (let
       (
        (scheme  (coerce (extract-scheme (coerce uri 'list)) 'string))
        )
     ( 
      if (isSpecial scheme)
      (special-scheme scheme after)
       (
        make-uri-struct
        :scheme scheme
        :userInfo (extract-userInfo after)
        :host (extract-host after)
        :port (extract-port after)
        :path (extract-path after)
        :query (extract-query after)
        :fragment (extract-fragment after)
        )
       )
     )
    (error "uri passed is not a string!")
    )
  )


;not already implemented functions:


;function that returns the schema if it is correct, throws an error instead.
(defun isSpecial (scheme)
  NIL
 )

;function called if it is a special scheme
(defun special-scheme (scheme after) "sas")

;function that returns the port if is not null, the default port if it is null
(defun get-port (scheme given-port)
  (if given-port
      given-port
      (cond
        ((string-equal scheme "http") 80)
        ((string-equal scheme "https") 443)
        ((string-equal scheme "ftp") 21)
        ((string-equal scheme "mailto") 25)
        ((string-equal scheme "news") 119)
        ((string-equal scheme "tel") nil)
        ((string-equal scheme "fax") nil)
        ((string-equal scheme "zos") 23)
        (t nil))))

(defun extract-scheme (chars)
  (cond ((null chars) (error "Schema is not valid"))
	(
         (string= (first chars) ":")
	 (defparameter after (rest chars))
         NIL
         )
	(T (if (identificatorep (first chars))
	       (append
		(list (first chars))
		(extract-scheme (rest chars)))
	       (error "invalid schema character")))))

(defun extract-userInfo (uri) "placeholder")

(defun extract-host (uri) "placeholder")

(defun extract-port (uri) "placeholder")

(defun extract-path (chars) 
  (
    cond 
    ((null chars) NIL)
    ((string= (first chars) "?") 
     (defparameter after (rest chars))
     (if (not (contains-single chars "?")) 
         (error "uri cannot contain more than 2 queries") 
       NIL
     ))
     
    (T 
     (if (identificatorep (first chars))
         (append (list (first chars)) (extract-path (rest chars)))
       (error "invalid path character ~c" (first chars))
       )
     )
    )
  )

(defun extract-query (uri) "placeholder")

(defun extract-fragment (uri) "placeholder")

(defun identificatorep (char)
  (or (alphanumericp char)
      (string= char "=")
      (string= char "@")
      (string= char ";")
      (string= char "/")
      ))

(defun contains-single (chars char2Check &optional (alreadyFound nil)) 
  (
   cond 
   ((null chars)alreadyFound)
   ((string= (first chars) char2Check)
    (
     if (equal alreadyFound T)
     nil
      (contains-single (rest chars) char2Check T)
     )
    )
   (T  (contains-single (rest chars) char2Check alreadyFound))
  )
)