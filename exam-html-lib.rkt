#lang at-exp racket

(require racket/contract
         "exam-lib-common.rkt"
         sxml)

(provide front-page
         344-blurb
         racket
         racketdisplay
         solution
         itemlist
         item
         multichoice
         emph
         texttt
         bold
         tt
         vspace
         verbatim
         rule
         table
         verb

         doc-element?)

;; these elements represent html
(define doc-element? sxml:element?)

(provide
 (contract-out
  [assemble-test (->* (string? any/c (listof (or/c prob-pair? newpage?)) path? string?)
                      (#:key boolean?
                       #:extra-pkgs (listof string?)
                       #:extra-page boolean?
                       #:print-problem-name boolean?)
                      boolean?)]
  #;[assemble-questions (-> (listof prob-pair?)
                          string?)]
  [question-names (-> (listof prob-pair?)
                      (listof symbol?))]
  [newpage false?]
  [problem (->* () #:rest (listof (or/c string? doc-element?))
                (-> real? doc-element?))]))


;; checks that it's a problem pair with a symbol and a document element
(define (prob-pair? pp)
  (and (problem-pair? pp)
       (symbol? (problem-pair-name pp))
       (doc-element? (problem-pair-text pp))))


;; given a list of problem-pairs and strings, return a list
;; of the problem names
(define (question-names quiz-list)
  (for/list ([question (in-list quiz-list)]
             #:when (not (newpage? question)))
    (cond [(problem-pair? question) (problem-pair-name question)]
          ;; I think we should just signal an error here:
          [else (error 'question-names
                       "no name for question: ~e" question)]
          #;[else "problem without a name"])))


;; given a list of problem-pairs and strings, return a single
;; string containing all of the questions.
(define (assemble-questions problems)
  (define problem-div
    (cons 'ol
          (map (λ (elt)
                 (list 'li elt))
               (map problem-pair-text problems))))
  ((sxml:modify (list "//solution" 'delete)) problem-div))

(define newpage #f)
(define newpage? false?)

;; given the pieces, assemble and pdf-slatex the file
(define (assemble-test title tex-blurb questions tgt-dir tgt-stem
                       #:key [key? #f]
                       #:extra-pkgs [extra-pkgs '()]
                       #:extra-page [extra-page? #f]
                       #:print-problem-name [print-problem-name? #f])
  ;; ignore the newpages
  (define prob-pairs (filter prob-pair? questions))
  ;; extra-page ignored, just there for uniformity with TeX back-end...
  ;; just check to make sure this doesn't signal an error:
  (question-names prob-pairs)
  ;; not yet implemented:
  (when print-problem-name?
    (error 'assemble-test "no support for print-problem-name option in html yet"))
  ;; something will have to happen with this...
  ;; (front-page title blurb extra-pkgs key?)
  (define doc-val
    `(html
      (head (|@| (title ,title)))
      (body ,(assemble-questions prob-pairs))))
  (define tgt-path (build-path tgt-dir (~a tgt-stem ".html")))
  (when (file-exists? tgt-path)
    (delete-file tgt-path))
  (srl:sxml->xml doc-val tgt-path)
  #t)


(define sa (λ args (error "don't call this")))

;; given the question text, produce
;; a function from the number of points to the tex for an exam problem
(define ((problem . strs-and-elts) pts)
  (define paragraph-eltses (split-into-paragraphs strs-and-elts))
  (define paragraphs (for/list ([elts (in-list paragraph-eltses)])
                       (cons 'p elts)))
  `(div
    (p "Points: " ,(~a pts))
    ,@paragraphs))

;; split at pairs of newlines.
(define (split-into-paragraphs strs-and-elts)
  (let loop ([remaining strs-and-elts]
             [reversed-so-far '()])
    (cond [(empty? remaining)
           (cond [(empty? reversed-so-far)
                  (list)]
                 [else (list (reverse reversed-so-far))])]
          [else
           (cond [(and (equal? (first remaining) "\n")
                       (not (empty? reversed-so-far))
                       (equal? (first reversed-so-far) "\n"))
                  (cons (reverse reversed-so-far)
                        (loop (rest remaining) '()))]
                 [else
                  (loop (rest remaining) (cons (first remaining)
                                               reversed-so-far))])])))

(check-equal? (split-into-paragraphs
               '("abc" "\n" "def" "\n" "\n" "ghi" "jbc" "\n" "a" "\n"))
              '(("abc" "\n" "def" "\n") ("ghi" "jbc" "\n" "a" "\n")))
(check-equal? (split-into-paragraphs
               '("\n" "abc" "\n" "def" "\n" "\n" "ghi" "jbc" "\n" "a" "\n"))
              '(("\n" "abc" "\n" "def" "\n") ("ghi" "jbc" "\n" "a" "\n")))
(check-equal? (split-into-paragraphs
               '("\n" "\n" "abc" "\n" "def" "\n" "\n" "ghi" "jbc" "\n" "a" "\n"))
              '(("\n") ("abc" "\n" "def" "\n")
                       ("ghi" "jbc" "\n" "a" "\n")))

;; wrap something in a schemedisplay environment
(define (racketdisplay . strs)
  (unless (andmap string? strs)
    (error 'racketdisplay "expected strings...."))
  ;; could be better, isn't...
  `(pre ,@strs))

;; verbatim
(define (verbatim . strs)
  (unless (andmap string? strs)
    (error 'verbatim "expected strings...."))
  `(pre ,@strs))

;; lilypond
;; OMG scary.
#;(define lilypond (env-thing "lilypond"))

;; a generic wrapper
(define (make-wrapper kwd)
  (unless (symbol? kwd)
    (error 'zzz "grr"))
  (lambda (str) (list kwd str)))

;; wrap something in a scheme argument
(define racket (make-wrapper 'code))
(define emph (make-wrapper 'i))
(define bold (make-wrapper 'bold))
(define texttt (make-wrapper 'code)) ;; better choice?
(define tt texttt)
(define vspace (λ args (error 'unimplemented "ouch")))

(define verb texttt)
;; rule
(define (rule width thickness)
  (error 'unimplemented "ouch222")
  (sa "\\rule{"width"}{"thickness"}"))

;; wrap something in the whole begsol endsol goo
(define (solution cms-of-space . strs)
  ;; resolve this at display time...
  `(solution ,cms-of-space ,@strs))

;;itemlist : yes, I'm faking the scribble forms, here:
(define (itemlist #:style [style #f] . items)
  (match style
    [#f (cons 'ul items)]
    ['ordered
     (cons 'ol items)]
    ['ordered-cont
     (error 'unimplemented "ouch-itemlist-ordered-cont")]
    ['alpha-ordered
     (error 'unimplemented "ouch-itemlist-alpha-ordered")]))

;; item: faking the scribble 'item'
(define (item . conts)
  (cons 'li conts))

;; given a number of columns and a bunch of items,
;; format them as enumerated items in the specified number
;; of columns
(define (multichoice [cols #f] . items)
  (error 'unimplemented "ouch-multichoice")
  #;((define real-cols (or cols (length items)))
  ((env-thing "multicols")
   #:arg (number->string real-cols)
   (apply (env-thing "enumerate")
          #:options "label=(\\alph*)"
          (map item items)))))


(require rackunit)

;; table... well, enough to get by. Heaven help you if 
;; you have an ampersand or a double-slash in there.
(define (table cols spec lorows)
  ;; ignoring spec, sigh
  (cons 'table
        (map (λ (r) (cons 'tr
                          (map (λ (cell)
                                 (list 'td cell))
                               r)))
             lorows)))


;; ignoring for now...
(define 344-blurb
  #<<|
\noindent\begin{minipage}{7.5cm} $\bullet$ Write down the answers in the
space provided.

$\bullet$ Simplify multiplication and division expressions.

$\bullet$ I will not answer any questions during the test, unless we discover
a problem that makes a question unsolvable or seriously broken.

\bigskip

{\em Good luck!\/}
\end{minipage}
|
  )

(define (pkgs pkg-strs)
  (error 'unimplemented "ouch-pkgs")
  #;(apply
   string-append
   (for/list ([p (in-list pkg-strs)])
     (string-append "\\usepackage{" p "}\n"))))

#;(check-equal? (pkgs '("abc" "def"))
              "\\usepackage{abc}\n\\usepackage{def}\n")

;; given the number of the exam and a string for the current quarter, produce a front page.
;; ultimately, it would be nice to turn TeX macros into racket
;; procedures.
(define (front-page title blurb extra-pkgs key?)
  (error 'unimplemented "yikes, ouch front-page")
  #;(@sa{\documentclass[11pt]{article}
\usepackage[margin=2.5cm,right=3.5cm]{geometry}
\usepackage{slatex}
@(pkgs extra-pkgs)

\defschememathescape{$}  %$
\setkeyword{define-struct define-type}
\setspecialsymbol{->}{$\rightarrow$}
\setspecialsymbol{lambda}{$\lambda$} 

%% ------------------------------------------------------------------
%% SOLUTIONS:
\def\thel{\noindent\rule{2.5cm}{.5pt}}
@(if key?
     @sa{\def\begsol#1{~\\ \thel {\bf Solution} \thel \\ }\def\endsol{~\\ \thel \\ }}
     @sa{\long\def\begsol#1 #2\endsol{#1}
})

%% PROBLEMS:
\def\pts#1{\marginpar{\footnotesize \raggedright  \fbox{#1 {\sc Points}}}}
\newcounter{problemnumber}
\newenvironment{problem}{\subproblem=0 \addtocounter{problemnumber}{1}
  \noindent{\bf Problem \theproblemnumber}\ }{\relax}

\newcount\subproblem
\subproblem=0
\newcommand {\newsubproblem} {\advance \subproblem by1 \noindent
  \textbf {\theproblemnumber .\the\subproblem } \hspace {0.5em}}

\newcounter{saveenumi}
\newcommand{\seti}{\setcounter{saveenumi}{\value{enumi}}}
\newcommand{\conti}{\setcounter{enumi}{\value{saveenumi}}}


%% ------------------------------------------------------------------

\begin{document}



%% ------------------------------------------------------------------

\vfill
\centerline{\Large @title}

~\\[2cm]

\begin{center}
\begin{tabular}@sa|{{l@{\qquad}l}}|
Name:  & \rule{200pt}{.1pt} \\[.5cm]
\end{tabular}
\end{center}

@blurb \hfil\begin{minipage}[t]{4.5cm}

@;{ \begin{tabular}{|c|l|@"@"/r|}
\hline
{\bf Problem} & Points & \\ \hline
1 & & 5\\ \hline
2 & & 10 \\ \hline
3 & & 15 \\ \hline
{\bf Total} & \relax & 30 \\ \hline
\end{tabular}
}\end{minipage}

\vfill\thispagestyle{empty}

\newpage
}))
