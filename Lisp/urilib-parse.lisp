
;;; Define a structure for representing a URI
(defstruct uri-struct
  scheme
  userInfo
  host
  port
  path
  query
  fragment
  )

;;; Accessor for the URI scheme
(defun urilib-scheme (uri) 
  (uri-struct-scheme uri)
  )

;;; Accessor for the user info component
(defun urilib-userinfo (uri) 
  (uri-struct-userinfo uri)
  )

;;; Accessor for the host component
(defun urilib-host (uri) 
  (uri-struct-host uri)
  )

;;; Accessor for the port component
(defun urilib-port (uri) 
  (uri-struct-port uri)
  )

;;; Accessor for the port component
(defun urilib-path (uri) 
  (uri-struct-path  uri)
  )

;;; Accessor for the query component
(defun urilib-query (uri) 
  (uri-struct-query uri)
  )

;;; Accessor for the fragment component
(defun urilib-fragment (uri) 
  (uri-struct-fragment uri)
  )

;;; Function to display the contents of a URI struct
(defun urilib-display (urilib-struct &optional (stream T)) 
  (if (not (typep urilib-struct 'URI-STRUCT))
      (error "incompatible type")
    (progn 
       ;; Format and print each component of the URI
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

;;; Parse a URI string and return a `uri-struct` instance
(defun urilib-parse (uri)
  (
   if (stringp uri) 
   (let*
       (
        (scheme  (coerce (car (extract-scheme (coerce uri 'list))) 'string))
        
        (rest-scheme (cdr (extract-scheme (coerce uri 'list))))

        
        (authority
         (cond
          ((isSpecial scheme) NIL)
          (
           (check-slashes rest-scheme) 
           (extract-authority (rest (rest rest-scheme)))
           )
          ((check-path-slashes rest-scheme) NIL)
          (T (error "invalid URI"))))


        (userInfo (if (contains-char rest-scheme  "@")                     
                      (extract-userInfo (rest (rest rest-scheme)))
                    (cons NIL (rest (rest rest-scheme)))
                    )
                  )


        (host (if (not (equal authority nil))
                  (check-host (cdr userInfo))
                (cons NIL (cdr userInfo))
                )
              )


        (port (if (string= (first (cdr host)) ":") 
                  (extract-port (rest (cdr host))) 
                (cons (get-port scheme NIL) (cdr host))
                )
              )


        (path (if (null (cdr port))
                  NIL
                (if (string= (first (cdr port)) "/")
                    (if (string= scheme "zos")
                        (zos-extract-path (rest (cdr port)))
                      (extract-path (rest (cdr port)))
                      )
                  (error "Missing '/'")
                  )
                  
                )
              )
        
        
        (query (if (string= (first (cdr path)) "?") 
                   (extract-query (rest (cdr path)))
                 (cons NIL (cdr path))
                 )
               )

        (fragment (if (string= (first (cdr query)) "#") 
                      (coerce (extract-fragment (rest (cdr query))) 'string) 
                    NIL
                    )
                  )
        
        )

     ( 
      ;; Construct the URI struct with parsed components and 
      ;; checks if the "schema" part is special 
      if (isSpecial scheme)
      (special-scheme scheme rest-scheme)
      (
       make-uri-struct
       :scheme scheme
       
       :userInfo (if (string= (coerce (car userInfo) 'string) "") 
                     NIL
                   (coerce (car userInfo) 'string)
                   )
       :host (coerce (car host) 'string)
       
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
    (error "uri passed is not a string!")
    )
  )


;;; Extract the scheme component from the URI
(defun extract-scheme (chars)
  (cond ((null chars) (error "Schema is not valid"))
	(
         (string= (first chars) ":")
         (cons NIL (rest chars))
         )
	(T (if (identificatorep (first chars))
               (let ((rest-scheme (extract-scheme (rest chars))))
                 (make-cons (first chars) rest-scheme)
                 )
             (error "invalid schema character")))))

;;; Determine if a scheme is considered special 
(defun isSpecial (scheme)
  (or (string= scheme "mailto")
      (string= scheme "news")
      (string= scheme "tel")
      (string= scheme "fax"))
  )

;;; Extract the authority component from the URI
(defun extract-authority (chars)
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
    (let ((rest-authority (extract-authority (rest chars))))
      (make-cons (first chars) rest-authority)
        ) 
    )
   )
  )

;;; Check if the URI starts with exactly two slashes
(defun check-slashes (chars) 
  (and 
   (string= (first chars) "/")
   (string= (second chars) "/")
   (not (string= (third chars) "/")))
  )

;;; Check if the URI starts with a single slash
(defun check-path-slashes (chars) 
  (or 
   (not (string= (first chars) "/"))
   (and 
    (string= (first chars) "/")
    (not( string= (second chars) "/"))))
  )

;;; Extract the user info component from the URI
(defun extract-userInfo (chars)
  (
   cond 
   ((null chars) NIL)
   ((string= (first chars) "@") 
    (cons NIL (rest chars))
    )
   (T 
    (if (identificatorep (first chars))
        (let ((rest-userInfo (extract-userInfo (rest chars))))
        (make-cons (first chars) rest-userInfo)
        )       
      (error "invalid userInfo character ~c" (first chars))
      )
    )
   )
 )



(defun make-cons (first rest) 
  (cons 
   (append
     (list first)
     (car rest)
     )
   (cdr rest)
   )
  )


;;; Extract the host component from the URI
(defun extract-host (chars &optional (isIP NIL))   
  (cond 
    ((null chars) NIL)
    
    ((or (string= (first chars) ":") (string= (first chars) "/"))
     (cons NIL chars)
     )
    


    ((string= (first chars) ".")
     (if isIP  ; se   un IP, accetta il punto senza controlli
         (make-cons (first chars) (extract-host (rest chars) isIP))
       
         ; se non   un IP, controlla che il carattere dopo sia alfabetico
         (if (alpha-char-p (first (rest chars)))
             (make-cons (first chars) (extract-host (rest chars) isIP))
             (error "extract host ip: invalid host character ~c" (first (rest chars)))
             )
         )
     )
    


    (T 
     (if (alphanumericp (first chars))
         (make-cons (first chars) (extract-host (rest chars) isIP))
       (error "extract host: invalid host character ~c" (first chars))
       )
     )
    )
  )



;;; Validate and extract the host component
(defun check-host (chars)
  (if (digit-char-p (first chars))
      ;If chars begins with a number
      (if (or (null (extract-host chars T)) (null (car (extract-host chars T))))
          (error "no host specified")
        (cons (check-ip (car (extract-host chars T))) (cdr (extract-host chars T)))
        )
      ;Else it's considered a normal host
    (if (or (null (extract-host chars)) (null (car (extract-host chars))))
        (error "no host specified")
      (extract-host chars)
          )
    )
  )



;;returns the IP address coerced to a string if it is valid, throws an error instead
(defun check-ip (chars)
  (let ((ip (extract-ip chars)))
    (if 
        (and
         (equal (length ip) 4)
         (every #'valid-ip-segment ip)
         )
        (coerce chars 'string)
      (error "ip entered not valid")
        )
    )
  )

;;returns T if a segment is valid NIL if it is not
(defun valid-ip-segment (segment)
  (let ((num (parse-integer segment)))
    (
     and
     (>= num 0)
     (<= num 255)
     (or (equal (length segment) 0) (not (string= (char segment 0) "0")))
     )
   )
  )

;;returns a list with all IP segments
(defun extract-ip (chars) 
  (let ((segment (extract-ip-segment chars)))
    (if (or (null segment) (null (car segment)))
        NIL
      (append (list (coerce (car segment) 'string)) (extract-ip (cdr segment)))
      )
    )
  )

;;returns the next segment of the ip address (all unntil the ".")
(defun extract-ip-segment (chars) 
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
         (let ((rest-IpSegment (extract-ip-segment (rest chars))))
           (make-cons (first chars) rest-IpSegment)
           )  
       (error "Ip address contains a not numeric value" (first chars))
       )
     )
    )
  )


;;; Extract the port component from the URI
(defun extract-port (chars)
  (
   cond 
   
   ;; If no characters remain, return NIL
   ((null chars) NIL)

   ;; Stop at the '/' character (indicating a path)
   ((string= (first chars) "/")
    (cons NIL chars)
     )
   
   ;; Validate numeric characters and continue extraction
   (T 
    (if (digit-char-p (first chars))
        ;(append (list (first chars)) (extract-port (rest chars)))
        (let ((rest-port (extract-port (rest chars))))
          (make-cons (first chars) rest-port)
          )
      (error "invalid port character ~c" (first chars))
      )
    )
   )
  )

;;; Get the port number for a given scheme or use a provided port
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
     (t 80))))

;;; Extract the path component from the URI
(defun extract-path (chars &optional (zos NIL)) 
  (
   cond 

   ;; If no characters remain, return NIL
    ((null chars) NIL)

    ;; Stop at '?' or '#' characters
    ((or (string= (first chars) "?") (string= (first chars) "#")) 
       (cons NIL chars)
       )

    ;; Validate the character and continue extraction
    (T 
     (if (or 
          (identificatorep (first chars)) 
          (string= (first chars) "/") 
          (and zos (or (string= (first chars) "(") (string= (first chars) ")") (string= (first chars) ".") ))
          )
         (let ((rest-path (extract-path (rest chars) zos)))
           (make-cons (first chars) rest-path)
           )
       (error "invalid path character ~c" (first chars))
       )
     )
    )
  )

;;; Validate and extract the zos-specific path
(defun zos-extract-path (chars)
  (let ((path (extract-path chars T)))
    (if (valid-zos-path (car path))
        path
      (error "invalid zos path")
     )
   )
  )

(defun valid-zos-path (chars) 
  (let* (
         (id44 (extract-id44 chars))
         (id8 (extract-id8 (rest (cdr id44))))
         )
    (and
     ;; Id44 conditions
     (and (alpha-char-p (first (car id44))) (every #'valid-id44-char-p (car id44)))

     ;; Id8 conditions
     (or (null id8)
         (and 
          (alpha-char-p (first (car id8))) 
          (every #'alphanumericp (car id8)) 
          (not (null (cdr id8)))
          )
         )
     )
    )
  )

(defun extract-id44 (chars) 
  (cond 
   ;; If no characters remain, return NIL
   ((null chars)  NIL)
   
   ;; Stop at '(' character
   ((string= (first chars) "(") 
    (cons NIL chars)
    )
   
   ;; Validate the character and continue extraction
   (T
    (let ((rest-id44 (extract-id44 (rest chars))))
      (make-cons (first chars) rest-id44)
      )
    )
   )
  )


(defun extract-id8 (chars) 
 (cond 
   ;; If no characters remain, return NIL
   ((null chars)  NIL)
   
   ;; Stop at ')' character
   ((string= (first chars) ")") 
    (cons NIL chars)
    )
   
   ;; Validate the character and continue extraction
   (T
    (let ((rest-id8 (extract-id8 (rest chars))))
      (make-cons (first chars) rest-id8)
      )
    )
   )
  )



(defun valid-id44-char-p (char)
  (or (alphanumericp char)
      (string= char ".")))




;;; Extract the query component from the URI
(defun extract-query (chars)
   (cond 
    
    ;; If no characters remain, return NIL
    ((null chars)  NIL)
    
    ;; Stop at '#' character
    ((string= (first chars) "#") 
     (cons NIL chars)
     )
    
    ;; Validate the character and continue extraction
    (T 
     (if (identificatorep (first chars))
         (let ((rest-query (extract-query (rest chars))))
           (make-cons (first chars) rest-query)
           )
       (error "invalid path character ~c" (first chars))
       )
     )
    )
   )

;;; Extract the fragment component from the URI
(defun extract-fragment (chars)
  (
   cond
   ;; If no characters remain, return NIL
   ((null chars) NIL)
   
   ;; Validate the character and continue extraction
   (T 
    (if (identificatorep (first chars))
        (append (list (first chars)) (extract-fragment (rest chars)))
      (error "invalid fragment character ~c" (first chars))
      )
    )
   )
  )

;;; Handle special schemes 
(defun special-scheme (scheme after) 
  (cond ((string= scheme "mailto")
          ;; Handle the "mailto" scheme
         (let ((schema scheme)
               (userinfo (coerce (extract-userinfo after) 'string))
               (host  (if (contains-char after "@")
                          (coerce (extract-host (rest after)) 'string)))
               (port (get-port scheme NIL)))
           ;; Raise an error if invalid characters are present
           (if (or (contains-char after "/")
                   (contains-char after "#")
                   (contains-char after "?"))
               (error "Non corretto")
             (make-uri-struct
              :scheme schema
              :userinfo userinfo
              :host host
              :port port))))
        ;; Handle the "news" scheme
        ((string= scheme "news")
         (let ((schema scheme)
               (host   (if (or (contains-char after "@")
                               (contains-char after ":"))
                           (error "Host non valido")
                         (coerce (extract-host after) 'string)))        
               (port (get-port scheme NIL)))
           ;; Raise an error if invalid characters are present
           (if (or (contains-char after "/")
                   (contains-char after "#")
                   (contains-char after "?"))
               (error "Non corretto")
             (make-uri-struct
              :scheme schema
              :host host
              :port port))))
        ;; Handle the "tel" and "fax" schemes
        ((or (string= scheme "tel") (string= scheme "fax"))      
         (if (every #'digit-char-p after)
             (make-uri-struct
              :scheme scheme
              :userinfo (coerce after 'string)
              :port (get-port scheme NIL))
           (error "Schema tel/fax non corretto"))
        )))
      

;;; Check if a character is a valid identifier character
(defun identificatorep (char)
  (or (alphanumericp char)
      (string= char "_")
      (string= char "=")
      (string= char "+")
      (string= char "-")
      ))
  

;;; Check if a character list contains the specified character
(defun contains-char (after char)
  (cond ((null after) NIL)
        ((string= (first after) char) T)
        (T (contains-char (rest after) char))))

;;; Ensure that the character list contains at most one occurrence of a character
(defun contains-at-most-one (chars char2Check &optional (alreadyFound nil)) 
  (
   cond 
   ((null chars) T)
   ((string= (first chars) char2Check)
    (
     if (equal alreadyFound T)
     nil
      (contains-at-most-one (rest chars) char2Check T)
     )
    )
   (T  (contains-at-most-one (rest chars) char2Check alreadyFound))
   ))

