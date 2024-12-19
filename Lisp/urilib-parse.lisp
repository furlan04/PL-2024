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

(defun urilib-display (uri &optional (stream T)) 
  (if (not (typep uri 'URI-STRUCT))
      (error "incompatible type")
      (progn 
        (format stream "Scheme: ~13T ~S ~%" (urilib-scheme uri) )
        (format stream "UserInfo: ~13T ~S ~%" (urilib-userInfo uri) )
        (format stream "Host: ~13T ~S ~%" (urilib-host uri) )
        (format stream "Port: ~13T ~S ~%" (urilib-port uri) )
        (format stream "Path: ~13T ~S ~%" (urilib-path uri) )
        (format stream "Query: ~13T ~S ~%" (urilib-query uri) )
        (format stream "Fragment: ~13T ~S" (urilib-fragment uri) )
        (if (not (equal stream T)) (close stream) t)
        )
   
      )
  )

(defun urilib-parse (uri)
  (
   if (stringp uri) 
   (let
       (
        (scheme (check-scheme (extract-scheme uri)))
        (userInfo (extract-userInfo uri))
        (host (extract-host uri))
        (port (extract-port uri))
        (path (extract-path uri))
        (query (extract-query uri))
        (fragment (extract-fragment uri))
      )
        ( 
         make-uri-struct
         :scheme scheme
         :userInfo userInfo
         :host host
         :port (get-port port)
         :path path
         :query query
         :fragment fragment
         ) )
    (error "uri passed is not a string!")
   )
  )


;not already implemented functions:


;function that returns the schema if it is correct, throws an error instead.
(defun check-scheme (scheme)
  (if
   (or (equal scheme "http")
        (equal scheme "https")
        (equal scheme "ftp")
        (equal scheme "mailto")
        (equal scheme "news")
        (equal scheme "tel")
        (equal scheme "fax")
        (equal scheme "zos"))
   scheme
   (error "the scheme is not valid!")
   )
 )

;function that returns the port if is not null, the default port if it is null
(defun get-port (port) port)

(defun extract-scheme (uri) "placeholder")

(defun extract-userInfo (uri) "placeholder")

(defun extract-host (uri) "placeholder")

(defun extract-port (uri) "placeholder")

(defun extract-path (uri) "placeholder")

(defun extract-query (uri) "placeholder")

(defun extract-fragment (uri) "placeholder")
