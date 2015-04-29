(defparameter *nodes* '
  ((wohnzimmer (du bist in einem wohnzimmer.
                   ein zauberer schnarcht laut auf dem sofa.))
  (garten (du bist in einem schoenen garten.
               vor dir befindet sich ein brunnen.))
  (dachboden (du bist auf dem dachboden.
                 in der ecke steht ein riesiger schweissbrenner.))))

(defparameter *edges* '((wohnzimmer (garten westen tuer)
                                    (dachboden oben leiter))
                        (garten (wohnzimmer osten tuer))
                        (dachboden (wohnzimmer unten leiter))))

(defparameter *objekte* '(whiskyflasche eimer frosch kette))

(defparameter *objekt-standorte* '((whiskyflasche wohnzimmer)
                                   (eimer wohnzimmer)
                                   (kette garten)
                                   (frosch garten)))

(defun beschreibe-standort (standort nodes)
  (cadr (assoc standort nodes)))

(defun beschreibe-pfad (edge)
  `(Hier kannst du eine ,(caddr edge) nach ,(cadr edge) benutzen.))

(defun beschreibe-pfade (standort edges)
  (apply #'append (mapcar #'beschreibe-pfad (cdr (assoc standort edges)))))

(defun objekte-an (ort objekte obj-ort)
  (labels ((an-ort-p (obj)
             (eq (cadr (assoc obj obj-ort)) ort)))
    (remove-if-not #'an-ort-p objekte)))

(defun beschreibe-objekte (ort objekte objekt-orte)
  (labels ((beschreibe-objekt (obj)
             `(du siehst das objekt ,obj auf dem boden.)))
    (apply #'append (mapcar #'beschreibe-objekt (objekte-an ort objekte objekt-orte)))))

(defparameter *standort* 'wohnzimmer)

(defun schaue ()
  (append (beschreibe-standort *standort* *nodes*)
          (beschreibe-pfade *standort* *edges*)
          (beschreibe-objekte *standort* *objekte* *objekt-standorte*)))

(defun gehe (richtung)
  (let ((next (find richtung
                    (cdr (assoc *standort* *edges*))
                    :key #'cadr)))
    (if next
      (progn (setf *standort* (car next))
             (schaue))
      '(du kannst nicht dorthin gehen.))))

(defun nimm (objekt)
  (cond ((member objekt
                 (objekte-an *standort* *objekte* *objekt-standorte*))
         (push (list objekt 'inventar) *objekt-standorte*)
         `(du hast das objekt ,objekt aufgenommen.))
        (t '(du kannst das nicht aufheben.))))

(defun inventar ()
  (cons 'objekte- (objekte-an 'inventar *objekte* *objekt-standorte*)))

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read ()
  (let ((cmd (read-from-string
               (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
             (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(schaue gehe nimm inventar))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
    (eval sexp)
    '(den befehl kenne ich nicht.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eq item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                                  (prin1-to-string lst))
                                     'list)
                             t
                             nil)
                 'string))
  (fresh-line))
