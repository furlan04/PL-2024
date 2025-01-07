

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
   (let
       (
        (scheme  (coerce (extract-scheme (coerce uri 'list)) 'string))
        (authority
         (cond
          ((isSpecial (coerce (extract-scheme (coerce uri 'list)) 'string)) NIL)
          ((check-slashes after) (extract-authority (rest (rest after))))
          ((check-path-slashes after) NIL)
          (T (error "invalid URI"))))
        
        )
     ( 
      ;; Construct the URI struct with parsed components and 
      ;; checks if the "schema" part is special 
      if (isSpecial scheme)
      (special-scheme scheme after)
       (
        make-uri-struct
        :scheme scheme
        :path (if (string= scheme "zos")
                  (if (string= (first after) "/") (coerce (zos-extract-path (rest after)) 'string) NIL)
                (if (string= (first after) "/") (coerce (extract-path (rest after)) 'string) NIL))
        :query (if (string= (first after) "?") (coerce (extract-query (rest after)) 'string) NIL)
        :fragment (if (string= (first after) "#") (coerce (extract-query (rest after)) 'string) NIL)
        :userInfo (cond
                   ((contains-char authority "@")
                    (let ((auth authority))                        
                    (coerce (extract-userInfo auth) 'string)))
                   (T (defparameter after authority)  NIL))
        :host (if (not (equal authority nil))
                  (if (string= (first after) "@")
                  (check-host (rest after))
                    (check-host after))              
                nil)
        :port (if (string= (first after) ":") (coerce (extract-port (rest after)) 'string) (get-port scheme NIL))
        )
       )
     )
    (error "uri passed is not a string!")
    )
  )

;;; Validate and extract the host component
(defun check-host (chars)
  (if (equal (extract-host chars) nil)
      (error "no host specified")    
    (if (digit-char-p (first chars))
        (check-ip (extract-host chars)) 
      (coerce (extract-host chars) 'string)))                                    
  )

;;; Extract the authority component from the URI
(defun extract-authority (chars)
  (
   cond 
   ( (or 
      (string= (first chars) "/")
      (string= (first chars) "?")
      (string= (first chars) "#")) 
     (defparameter after chars)
     NIL)
   ( (null chars)
     (defparameter after NIL)
     NIL)   
   ( T 
     (append (list (first chars))
             (extract-authority (rest chars)))
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
  (and 
   (string= (first chars) "/")
   (not( string= (second chars) "/")))
  )

;;; Determine if a scheme is considered special 
(defun isSpecial (scheme)
  (or (string= scheme "mailto")
      (string= scheme "news")
      (string= scheme "tel")
      (string= scheme "fax"))
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

;;; Extract the scheme component from the URI
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

;;; Extract the user info component from the URI
(defun extract-userinfo (chars)
  (
   cond 
   ((null chars) NIL)
   ((string= (first chars) "@") 
    (defparameter after  chars)
    NIL
    )
   (T 
    (if (identificatorep (first chars))
        (append (list (first chars)) (extract-userinfo (rest chars)))
      (error "invalid userInfo character ~c" (first chars))
      )
    )
   )
 )

;;; Extract the host component from the URI
(defun extract-host (chars) 
  (
   cond 
   ;; If no characters remain, return NIL
    ((null chars) NIL)
    ;; Stop at the ':' character (indicating a port)
    ((string= (first chars) ":") 
     (defparameter after chars)
     (if (not (contains-at-most-one chars ":")) 
         (error "uri cannot contain more than 1 port") 
       NIL
       ))
    ;; Validate the character and continue extraction
    (T 
     (if (identificatorep (first chars))
         (append (list (first chars)) (extract-host (rest chars)))
       (error "extract host: invalid host character ~c" (first chars))
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
   ;; Validate numeric characters and continue extraction
   (T 
    (if (digit-char-p (first chars))
        (append (list (first chars)) (extract-port (rest chars)))
      (error "invalid fragment character ~c" (first chars))
      )
    )
   )
  )

;;; Validate and extract the zos-specific path
(defun zos-extract-path (chars)
  (let((zos-path (extract-path chars)))
    (if (valid-zos-path-p zos-path)
        zos-path
      (error "path zos non corretta")))  
  )

;;; Validate the ZOS path format
(defun valid-zos-path-p (char-list)
  (labels ((alphanumeric-char-p (char)
             (or (alpha-char-p char)
                 (digit-char-p char)))
          
          (valid-id44-char-p (char)
             (or (alphanumeric-char-p char)
                 (char= char #\.)))
          
          (parse-id8 (chars)
             (and chars  
                  (char= (first chars) #\()
                  (let* ((closing-pos (position #\) chars))
                         (id8-chars (when closing-pos 
                                    (subseq chars 1 closing-pos))))
                    (and closing-pos
                         (= (1+ closing-pos) (length chars))  
                         id8-chars
                         (<= (length id8-chars) 8)
                         (every #'alphanumeric-char-p id8-chars)))))
          
          (find-id8-start (chars)
             (position #\( chars)))
    
    (let* ((id8-start (find-id8-start char-list))
           (id44-part (if id8-start 
                         (subseq char-list 0 id8-start)
                         char-list))
           (id8-part (when id8-start 
                      (subseq char-list id8-start))))
      
      (and 
       (<= (length id44-part) 44)
       (not (null id44-part))  
       (every #'valid-id44-char-p id44-part)
       (or (null id8-part)  
           (parse-id8 id8-part))))))

;;; Extract the path component from the URI
(defun extract-path (chars) 
  (
   cond 
    ((null chars) NIL)
    ((or (string= (first chars) "?") (string= (first chars) "#")) 
     (defparameter after chars)
     (if (and (not (contains-at-most-one chars "?")) (not (contains-at-most-one chars "#"))) 
         (error "uri cannot contain more than 1 query or fragment") 
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

;;; Extract the query component from the URI
(defun extract-query (chars)
  (
   cond 
   ((null chars) NIL)
   ((string= (first chars) "#") 
    (defparameter after chars)
    (if (not (contains-at-most-one chars "#")) 
        (error "uri cannot contain more than 1 fragment") 
      NIL
      ))
   
   (T 
    (if (identificatorep (first chars))
        (append (list (first chars)) (extract-path (rest chars)))
      (error "invalid query character ~c" (first chars))
      )
    )
   )
  )

;;; Extract the fragment component from the URI
(defun extract-fragment (chars)
  (
   cond 
   ((null chars) NIL)
   (T 
    (if (identificatorep (first chars))
        (append (list (first chars)) (extract-path (rest chars)))
      (error "invalid fragment character ~c" (first chars))
      )
    )
   )
  )

;;; Check if a character is a valid identifier character
(defun identificatorep (char)
  (or (alphanumericp char)
      (string= char "_")
      (string= char "=")
      (string= char "+")
      (string= char "-")
      (string= char ".")
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

;;; Handle special schemes 
(defun special-scheme (scheme after) 
  (cond ((string= scheme "mailto")
         (let ((schema scheme)
               (userinfo (coerce (extract-userinfo after) 'string))
               (host  (if (contains-char after "@")
                          (coerce (extract-host (rest after)) 'string)))
               (port (get-port scheme NIL)))
           (if (or (contains-char after "/")
                   (contains-char after "#")
                   (contains-char after "?"))
               (error "Non corretto")
             (make-uri-struct
              :scheme schema
              :userinfo userinfo
              :host host
              :port port))))
        
        ((string= scheme "news")
         (let ((schema scheme)
               (userinfo (coerce (extract-userinfo after) 'string))
               (host   (if (or (contains-char after "@")
                               (contains-char after ":"))
                           (error "Host non valido")
                         (coerce (extract-host after) 'string)))        
               (port (get-port scheme NIL)))
           (if (or (contains-char after "/")
                   (contains-char after "#")
                   (contains-char after "?"))
               (error "Non corretto")
             (make-uri-struct
              :scheme schema
              :userinfo userinfo
              :host host
              :port port))))
        
        ((or (string= scheme "tel") (string= scheme "fax"))
         (let ((schema scheme)
               (userinfo (coerce (extract-userinfo after) 'string))
               (host (if (or (contains-char after "@"))
                         (error "Host non valido")
                       (coerce (extract-host after) 'string)))        
               (port (get-port scheme NIL)))
           (if (or (contains-char after "/")
                   (contains-char after "#")
                   (contains-char after "?"))
               (error "Non corretto")
             (make-uri-struct
              :scheme schema
              :userinfo userinfo
              :host host
              :port port))))
        ))
       
;;returns the next segment of the ip address (all unntil the ".")
(defun extract-ip-segment (chars) 
  (
   cond 
    ((null chars) 
     (defparameter ip-after NIL)
     NIL)
    ((string= (first chars) ".") 
     (defparameter ip-after (rest chars))
     NIL
     )
    (T 
     (if (digit-char-p (first chars))
         (append (list (first chars)) (extract-ip-segment (rest chars)))
       (error "Ip address contains a not numeric value" (first chars))
       )
     )
    )
  )


;;returns a list with all IP segments
(defun extract-ip (chars) 
  (let ((segment (extract-ip-segment chars)))
    (if (equal NIL segment)
        NIL
      (append (list (coerce segment 'string)) (extract-ip ip-after))
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