;;;; Urilib-parse.lisp starts here

;;;; URI Parsing Library
;;;;
;;;; Description:
;;;; This library provides utilities for parsing, extracting, and validating
;;;; components of Uniform Resource Identifiers (URIs) (almost) according to
;;;; established standards (e.g., RFC 3986), look at readme.md  
;;;; for mor specific clarifications on the standard.
;;;;
;;;; Features:
;;;; - Extraction of specific URI components such as scheme, userInfo, 
;;;;   host, path, and fragment.
;;;; - Validation of URI syntax and structure.
;;;;
;;;; Usage:
;;;; - Call the function (urilib-parse <URI string>) to get a 
;;;;   validated urilib-structure-object.
;;;; - Call the function (urilib-display <urilib-structure>)
;;;;   to display your structure.
;;;; - Call the function (urilib-#component# <urilib-structure>)
;;;;   to get a URI component.
;;;;   Replace component with the name of the component 
;;;;   you want to get such as scheme, userInfo... 
;;;;
;;;; Authors:
;;;; [Simone Monzardo, Gabriele Furlan, Gabriele Beretta]


;;; Structure representing  the URI
(defstruct urilib-structure
  scheme
  userInfo
  host
  port
  path
  query
  fragment
  )

;;; Parse and validate an URI, return a urilib-structure object
(defun urilib-parse (uri)
  (
   
   if (stringp uri) 
   (if (is-special-scheme-p (coerce 
                             (car (parse-scheme (coerce uri 'list))) 
                             'string))
       (parse-special-scheme (coerce (car (parse-scheme (coerce uri 'list))) 
                                     'string) 
                             (cdr (parse-scheme (coerce uri 'list)))
                             )
     (let*
         (
          (scheme  (coerce (car (parse-scheme (coerce uri 'list))) 
                           'string))       
          (rest-scheme (cdr (parse-scheme (coerce uri 'list))))        
          (authority
           (cond
            ((is-special-scheme-p scheme) NIL)
            (
             (valid-slashes-p rest-scheme) 
             (parse-authority (rest (rest rest-scheme)))
             )
            ((valid-path-only-slashes-p rest-scheme) NIL)
            (T (error "urilib-parse:::urilib-parse::error:
invalid URI entered"))))
          (userInfo (if (list-contains-char rest-scheme  "@")                     
                        (parse-userInfo (rest (rest rest-scheme)))
                      (cons NIL (rest (rest rest-scheme)))
                      )
                    )
          (host (if (not (null authority))
                    (valid-host-p (cdr userInfo))
                  (cons NIL (cdr userInfo))
                  )
                )
          (port (if (string= (first (cdr host)) ":") 
                    (parse-port (rest (cdr host))) 
                  (cons (get-default-port scheme NIL) (cdr host))
                  )
                )
          (path (let ((after-scheme (if (null authority) 
                                        rest-scheme
                                      (cdr port)
                                      )
                                    )
                      )
                  (if (null after-scheme)
                      NIL
                    (cond 
                     ((string= (first after-scheme) "/")
                      (if (string= scheme "zos")
                          (parse-zos-path (rest after-scheme))
                        (parse-path (rest after-scheme))
                        ))
                     ((and (not (string= (first after-scheme) "/"))
                           (null authority))
                      (if (string= scheme "zos")
                          (parse-zos-path after-scheme)
                        (parse-path after-scheme)
                        ))
                     (T (error "urilib-parse:::urilib-parse::error:
missing '/' before path, query or fragment segment"))
                     )
                    )       
                  )
                )     
          (query (if (string= (first (cdr path)) "?") 
                     (parse-query (rest (cdr path)))
                   (cons NIL (cdr path))
                   )
                 )
          (fragment (if (string= (first (cdr query)) "#") 
                        (coerce (parse-fragment (rest (cdr query))) 
                                'string) 
                      NIL
                      )
                    )        
          )
       ( 
        
        
        make-urilib-structure
        :scheme scheme      
        :userInfo (if (string= (coerce (car userInfo) 'string) "") 
                      NIL
                    (coerce (car userInfo) 'string)
                    )
        :host (if (string= (coerce (car host) 'string)  "")
                  NIL
                (coerce (car host) 'string)
                )       
        :port (if (listp (car port))
                  (coerce (car port) 'string)
                (car port)
                )       
        :path (if (string= (coerce (car path) 'string)  "")
                  NIL
                (coerce (car path) 'string)
                )
        :query (if (string= (coerce (car query) 'string) "")
                   NIL
                 (coerce (car query) 'string)
                 )
        :fragment fragment      
        )
       )
     )
    (error "urilib-parse:::urilib-parse::error: 
uri passed is not a string!")
    )
  )


;;; Parse URI in case it is a special scheme
(defun parse-special-scheme (scheme after-scheme) 
  (cond ((string= scheme "mailto")
         (let* ((userInfo (parse-userInfo after-scheme))
                (host  (if (list-contains-char after-scheme "@")
                           (coerce (car (valid-host-p  (cdr userInfo))) 
                                   'string)))
                (port (get-default-port scheme NIL)))
           (if (or (list-contains-char after-scheme "/")
                   (list-contains-char after-scheme "#")
                   (list-contains-char after-scheme "?"))
               (error "urilib-parse:::urilib-special-scheme::error:
mailto tpye uri entered is not valid")
             (make-urilib-structure
              :scheme scheme
              :userinfo (coerce (car userInfo) 'string)
              :host host
              :port port
              )
             )
           )
         )
        ((string= scheme "news")
         (let (
               (host   (if (or (list-contains-char after-scheme "@")
                               (list-contains-char after-scheme ":"))
                           (error "urilib-parse:::urilib-special-scheme::error:
news tpye uri is invalid: invalid host specified")
                         (coerce (car (valid-host-p after-scheme)) 'string)))        
               (port (get-default-port scheme NIL)))
           (if (or (list-contains-char after-scheme "/")
                   (list-contains-char after-scheme "#")
                   (list-contains-char after-scheme "?"))
               (error "urilib-parse:::urilib-special-scheme::error:
news tpye uri is invalid")
             (make-urilib-structure
              :scheme scheme
              :host host
              :port port
              )
             )
           )
         )
        
        ((or (string= scheme "tel") (string= scheme "fax"))      
         (if (every #'digit-char-p after-scheme)
             (make-urilib-structure
              :scheme scheme
              :userinfo (coerce after-scheme 'string)
              :port (get-default-port scheme NIL))
           (error "urilib-parse:::urilib-special-scheme::error:
tel/fax tpye uri is invalid"))
         )
        )
  )


;;; Getters functions starts here


(defun urilib-scheme (uri) 
  (urilib-structure-scheme uri)
  )


(defun urilib-userinfo (uri) 
  (urilib-structure-userinfo uri)
  )


(defun urilib-host (uri) 
  (urilib-structure-host uri)
  )


(defun urilib-port (uri) 
  (urilib-structure-port uri)
  )


(defun urilib-path (uri) 
  (urilib-structure-path  uri)
  )


(defun urilib-query (uri) 
  (urilib-structure-query uri)
  )


(defun urilib-fragment (uri) 
  (urilib-structure-fragment uri)
  )

;;; Getters functions ends here


;;; Display a urilib-structure object
(defun urilib-display (urilib-struct &optional (stream T)) 
  (if (not (typep urilib-struct 'urilib-structure))
      (error "urilib-parse:::urilib-display::error:
parameter passed is not of type <urilib-structure>")
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


;;; Get the scheme component as a list
;;; from the URI (passed as a list)
(defun parse-scheme (chars)
  (cond ((null chars) (error "urilib-parse:::parse-scheme::error:
no valid scheme found"))
	(
         (string= (first chars) ":")
         (cons NIL (rest chars))
         )
	(T (if (identificatorep (first chars))
               (let ((rest-scheme (parse-scheme (rest chars))))
                 (append-remaining (first chars) rest-scheme)
                 )
             (error "urilib-parse:::parse-scheme::error:
invalid scheme character ~c" 
                    (first chars)
                    )
             )
           )
        )
  )


;;; Check if a URI as a special scheme
(defun is-special-scheme-p (scheme)
  (or (string= scheme "news")
      (string= scheme "mailto")
      (string= scheme "fax")
      (string= scheme "tel"))
  )


;;; Get the authority section as a list
;;; from the URI (passed as a list)
(defun parse-authority (chars)
  (
   cond 
   ((or 
     (null chars)
     (string= (first chars) "/")
     (string= (first chars) "?")
     (string= (first chars) "#")) 
    (cons NIL chars)
    )
   
   (T 
    (let ((rest-authority (parse-authority (rest chars))))
      (append-remaining (first chars) rest-authority)
      ) 
    )
   )
  )


;;; Validate the slashes section after the scheme
;;; in case an authority exists
(defun valid-slashes-p (chars) 
  (and 
   (string= (first chars) "/")
   (string= (second chars) "/")
   (not (string= (third chars) "/"))
   )
  )

;;; Validate the slashes section after the scheme
;;; in case an authority does not exist
(defun valid-path-only-slashes-p (chars) 
  (or 
   (not (string= (first chars) "/"))
   (and 
    (string= (first chars) "/")
    (not( string= (second chars) "/"))
    )
   )
  )


;;; Get the userInfo component as a list
;;; from the URI (passed as a list)
(defun parse-userInfo (chars)
  (
   cond 
   ((null chars) NIL)
   ((string= (first chars) "@") 
    (cons NIL (rest chars))
    )
   (T 
    (if (identificatorep (first chars))
        (let ((rest-userInfo (parse-userInfo (rest chars))))
          (append-remaining (first chars) rest-userInfo)
          )       
      (error "urilib-parse:::parse-userInfo::error:
invalid userInfo character ~c" 
             (first chars)
             )
      )
    )
   )
  )


;;; Create a cons for parser funtions that returns 
;;; a cons with appended results and remaining list
(defun append-remaining (first rest) 
  (cons 
   (append
    (list first)
    (car rest)
    )
   (cdr rest)
   )
  )



;;; Get the host component as a list
;;; from the URI (passed as a list)
(defun parse-host (chars &optional (isIP NIL))   
  (cond 
   ((null chars) NIL)    
   ((or (string= (first chars) ":") (string= (first chars) "/"))
    (cons NIL chars)
    )
   ((string= (first chars) ".")
    (if isIP 
        (append-remaining (first chars) (parse-host (rest chars) isIP))
      (if (alpha-char-p (first (rest chars)))
          (append-remaining (first chars) (parse-host (rest chars) isIP))
        (error "urilib-parse:::parse-host::error:invalid host character ~c"
               (first (rest chars)))
        )
      )
    )
   (T 
    (if (alphanumericp (first chars))
        (append-remaining (first chars) (parse-host (rest chars) isIP))
      (error "urilib-parse:::parse-host::error:invalid host character ~c"
             (first (rest chars)))
      )
    )
   )
  )


;;; Get the host component as a list
;;; in case it is not an IP address
;;; from the URI (passed as a list)
;;; and validate it with additional controls
(defun valid-host-p (chars)
  (if (digit-char-p (first chars))
    
      (if (or (null (parse-host chars T)) (null (car (parse-host chars T))))
          (error "urilib-parse:::valid-host-p::error:
no host specified")
        (cons (valid-ip-p (car (parse-host chars T))) 
              (cdr (parse-host chars T)))
        )
      
    (if (or (null (parse-host chars)) (null (car (parse-host chars))))
        (error "urilib-parse:::valid-host-p::error:
no host specified")
      (parse-host chars)
      )
    )
  )



;;; Get the host component as a list
;;; in case it is an IP address
;;; from the URI (passed as a list)
;;; and validate it with additional controls
(defun valid-ip-p (chars)
  (let ((ip (parse-ip chars)))
    (if 
        (and
         (equal (length ip) 4)
         (every #'valid-ip-segment-p ip)
         )
        (coerce chars 'string)
      (error "urilib-parse:::valid-ip-p::error:
ip entered not valid")
      )
    )
  )

;;; check if an ip segment is valid or not
(defun valid-ip-segment-p (segment)
  (let ((num (parse-integer segment)))
    (
     and
     (>= num 0)
     (<= num 255)
     (or (equal (length segment) 0) (not (string= (char segment 0) "0")))
     )
    )
  )

;;; get a list of strings 
;;; where each element is a segment of an IP address
;;; from a list of characters containing the ip address
(defun parse-ip (chars) 
  (let ((segment (parse-ip-segment chars)))
    (if (or (null segment) (null (car segment)))
        NIL
      (append (list (coerce (car segment) 'string)) 
              (parse-ip (cdr segment)))
      )
    )
  )

;;; get the following segment of the Ip address
(defun parse-ip-segment (chars) 
  (
   cond 
   ((null chars) 
    (cons NIL NIl)
    )
   ((string= (first chars) ".") 
    (cons NIL (rest chars))
    )
   (T 
    (if (digit-char-p (first chars))
        (let ((rest-IpSegment (parse-ip-segment (rest chars))))
          (append-remaining (first chars) rest-IpSegment)
          )  
      (error "urilib-parse:::parse-ip-segment::error:
ip address contains a not numeric value" (first chars))
      )
    )
   )
  )


;;; Get the port component as a list
;;; from the URI (passed as a list)
(defun parse-port (chars)
  (
   cond 
   ((null chars) NIL)

   ((string= (first chars) "/")
    (cons NIL chars)
    )

   (T 
    (if (digit-char-p (first chars))

        (let ((rest-port (parse-port (rest chars))))
          (append-remaining (first chars) rest-port)
          )
      (error "urilib-parse:::parse-port::error:
invalid port character ~c" (first chars))
      )
    )
   )
  )

;;; Get the default port number of the passed protocol scheme
;;; use the specified port if it is not NIL
(defun get-default-port (scheme specified-port)
  (if specified-port
      specified-port
    (cond
     ((string-equal scheme "http") 80)
     ((string-equal scheme "https") 443)
     ((string-equal scheme "ftp") 21)
     ((string-equal scheme "mailto") 25)
     ((string-equal scheme "news") 119)
     ((string-equal scheme "tel") nil)
     ((string-equal scheme "fax") nil)
     ((string-equal scheme "zos") 23)
     (t 80))))



;;; Get the path component as a list
;;; in case it is not a zos scheme path
;;; from the URI (passed as a list)
(defun parse-path (chars &optional (zos NIL)) 
  (
   cond 
 
 
   ((null chars) NIL)
   
   
   ((or (string= (first chars) "?") (string= (first chars) "#")) 
    (cons NIL chars)
    )
   

   (T 
    (if (or 
         (identificatorep (first chars)) 
         (string= (first chars) "/") 
         (and zos (or 
                   (string= (first chars) "(") 
                   (string= (first chars) ")") 
                   (string= (first chars) ".") 
                   )
              )
         )
        (let ((rest-path (parse-path (rest chars) zos)))
          (append-remaining (first chars) rest-path)
          )
      (error "urilib-parse:::parse-path::error:
invalid path character ~c" (first chars))
      )
    )
   )
  )



;;; Get the path component as a list
;;; in case it is a zos scheme path
;;; from the URI (passed as a list)
;;; and validate it with additional controls
(defun parse-zos-path (chars)
  (let ((path (parse-path chars T)))
    (if (valid-zos-path-p (car path))
        path
      (error "urilib:::parse-zos-path::error:
invalid zos path")
      )
    )
  )

;;; perform additional controls on the zos path
(defun valid-zos-path-p (chars) 
  (let* (
         (id44 (parse-id44 chars))
         (id8 (parse-id8 (rest (cdr id44))))
         )
    (and

     (and (alpha-char-p (first (car id44))) 
          (every #'valid-id44-char-p (car id44))
          (<= (length (car id44)) 44)
          )

     (or (null id8)
         (and 
          (alpha-char-p (first (car id8))) 
          (every #'alphanumericp (car id8))
          (<= (length (car id8)) 8)
          (not (null (cdr id8)))
          )
         )
     )
    )
  )

;;; Get the id44 component of the zos path  as a list
;;; from the path (passed as a list)
(defun parse-id44 (chars) 
  (cond 
 
   ((null chars)  NIL)
   
  
   ((string= (first chars) "(") 
    (cons NIL chars)
    )
   
  
   (T
    (let ((rest-id44 (parse-id44 (rest chars))))
      (append-remaining (first chars) rest-id44)
      )
    )
   )
  )


;;; Get the id8 component of the zos path  as a list
;;; from the path (passed as a list)
(defun parse-id8 (chars) 
  (cond 

   ((null chars)  NIL)
   

   ((string= (first chars) ")") 
    (cons NIL chars)
    )
 
   (T
    (let ((rest-id8 (parse-id8 (rest chars))))
      (append-remaining (first chars) rest-id8)
      )
    )
   )
  )


;;; Perform chacters control on id44 segment
(defun valid-id44-char-p (char)
  (or (alphanumericp char)
      (string= char ".")))




;;; Get the query component as a list
;;; from the URI (passed as a list)
(defun parse-query (chars)
  (cond 
   
   ((null chars)  NIL)
   
   ((string= (first chars) "#") 
    (cons NIL chars)
    )
   
   (T 
    (if (identificatorep (first chars))
        (let ((rest-query (parse-query (rest chars))))
          (append-remaining (first chars) rest-query)
          )
      (error "urilib-parse:::parse-query::error:
invalid path character ~c" (first chars))
      )
    )
   )
  )

;;; Get the fragment component as a list
;;; from the URI (passed as a list)
(defun parse-fragment (chars)
  (
   cond

   ((null chars) NIL)
   

   (T 
    (if (identificatorep (first chars))
        (append (list (first chars)) (parse-fragment (rest chars)))
      (error "urilib-parse:::parse-fragment::error:
invalid fragment character ~c" (first chars))
      )
    )
   )
  )



;;; Perform controls on characters
;;; Says if a character is admitted or not
(defun identificatorep (char)
  (or (alphanumericp char)
      (string= char "_")
      (string= char "=")
      (string= char "+")
      (string= char "-")
      )
  )


;;; Says if a character list contains or not
;;; a specified character.
(defun list-contains-char (list char)
  (cond ((null list) NIL)
        ((string= (first list) char) T)
        (T (list-contains-char (rest list) char))))


;;;; urilib-parse.lisp ends here