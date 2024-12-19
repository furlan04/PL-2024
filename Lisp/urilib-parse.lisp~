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

(defun create-struct-temp (scheme userInfo host port path query fragment) 
  (make-uri-struct :scheme scheme :userInfo userInfo
                    :host host :port port :path path :query query :fragment fragment)
  )

(defun urilib-display (uri &optional (stream T)) 
  (if (not (typep uri 'URI-STRUCT))
      (error "Urilib::urilib-display::error: incompatible type")
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
