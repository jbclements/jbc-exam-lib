#lang at-exp racket

(require racket/contract
         slatex/slatex-wrapper)

(provide front-page
         344-blurb
         racket
         piperacket
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
         doc-element?
         centering
         lstlisting
         listings-package-setup)

(define doc-element? string?)

(provide
 (contract-out
  [assemble-test (->* (string? doc-element? (listof (or/c prob-pair? string?))
                               path? string?)
                      (#:key boolean?
                       #:extra-pkgs (listof string?)
                       #:extra-page boolean?
                       #:title-page-break boolean?
                       #:print-problem-name boolean?
                       #:pkg-config-strs (listof string?))
                      
                      boolean?)]
  #;[assemble-questions (-> (listof prob-pair?)
                          string?)]
  [question-names (-> (listof (or/c prob-pair? string?))
                      (listof symbol?))]
  [newpage string?]
  [problem (->* () #:rest (listof doc-element?) (-> real? doc-element?))]))


;; checks that it's a problem pair with a symbol and a document element
(define (prob-pair? pp)
  (and (problem-pair? pp)
       (symbol? (problem-pair-name pp))
       (doc-element? (problem-pair-text pp))))

;; represents a pair including the name of the problem and the
;; text of the problem. Must match the definition in 2 other places
(struct problem-pair (name text tags) #:prefab)

;; given a list of problem-pairs and strings, return a list
;; of the problem names
(define (question-names quiz-list)
  (for/list ([question (in-list quiz-list)]
             #:when (not (string? question)))
    (problem-pair-name question)))


;; given a list of problem-pairs and strings, return a single
;; string containing all of the questions.
(define (assemble-questions problems print-problem-name?)
  (apply
   string-append
   (for/list ([p (in-list problems)])
     (cond
       [(string? p) p]
       [else (string-append
              (cond [print-problem-name?
                     (string-append "\\textbf{"
                                    (~a (problem-pair-name p))
                                    "} ")]
                    [else ""])
              (problem-pair-text p))]))))

(define newpage "\\newpage\n")
(define (newpage? s)
  (or (equal? s newpage)
      (equal? s "\\newpage")))

;; given the pieces, assemble and pdf-slatex the file
(define (assemble-test title blurb questions file-path file-stem
                       #:key [key? #f]
                       #:extra-pkgs [extra-pkgs '()]
                       #:extra-page [extra-page? #f]
                       #:title-page-break [title-page-break? #t]
                       #:print-problem-name [print-problem-name? #f]
                       #:pkg-config-strs [package-config-strs '()])
  ;; just check to make sure this doesn't signal an error:
  (question-names (filter prob-pair? questions))
  (define target-path (build-path file-path (~a file-stem ".tex")))
  (display-to-file 
   (string-append (front-page title blurb extra-pkgs package-config-strs key?)
                  (cond [title-page-break? "\n\\newpage\n"]
                        [else ""])
                  (assemble-questions questions print-problem-name?)
                  (if extra-page? (string-append newpage " ~") "")
                  @string-append{\end{document}})
   target-path
   #:exists 'truncate)
  (pdf-slatex (path->string target-path)))


(define sa string-append)

;; given the question text, produce
;; a function from the number of points to the tex for an exam problem
(define ((problem . strs) pts)
  (sa
   @sa{\pts{@number->string[pts]}
                  \begin{problem}

}
   (apply sa strs)
   @sa{
  
\end{problem}}))

;; a helper function for defining environment-like things
(define ((env-thing kind) #:options [opt-str #f] #:arg [arg-str #f] . conts)
  (define content (apply sa conts))
  (define opt (cond [opt-str @sa{[@opt-str]}]
                    [else ""]))
  (define arg (cond [arg-str @sa{{@arg-str}}]
                    [else ""]))
  @sa{\begin{@kind}@|opt|@|arg|
@content
\end{@kind}

})

;; wrap something in a schemedisplay environment
(define racketdisplay (env-thing "schemedisplay"))

(define-syntax (make-samename-env-thing stx)
  (syntax-case stx ()
    [(_ id)
     #`(define id (env-thing #,(symbol->string (syntax-e #'id))))]))

(make-samename-env-thing verbatim)
(make-samename-env-thing lilypond)
(make-samename-env-thing centering)
(make-samename-env-thing lstlisting)

;; a generic wrapper
(define (make-wrapper kwd)
  (lambda strs
    (when (not (andmap string? strs))
      (error 'wrapper-fun "expected strings, got ~e, kwd: ~e"
             strs kwd))
    (sa "\\" kwd "{" (apply sa strs) "}")))

;; a pipe-wrapper
(define (make-pipe-wrapper kwd)
  (lambda strs
    (when (not (andmap string? strs))
      (error 'wrapper-fun "expected strings, got ~e, kwd: ~e"
             strs kwd))
    (sa "\\" kwd "|" (apply sa strs) "|")))

;; wrap something in a scheme argument
(define racket (make-wrapper "scheme"))
(define piperacket (make-pipe-wrapper "scheme"))
(define emph (make-wrapper "emph"))
(define bold (make-wrapper "textbf"))
(define texttt (make-wrapper "texttt"))
(define tt texttt)
(define vspace (make-wrapper "vspace"))

;; this # is going to bite me hard some day...
(define (verb . strs)
  (sa "\\verb#" (apply sa strs)"#"))
;; rule
(define (rule width thickness)
  (sa "\\rule{"width"}{"thickness"}"))

;; wrap something in the whole begsol endsol goo
(define (solution cms-of-space . strs)
  @sa{\begsol{\vspace{@(number->string cms-of-space) cm}}
@(apply sa strs)
\endsol

})

;;itemlist : yes, I'm faking the scribble forms, here:
(define (itemlist #:style [style #f] . items)
  (match style
    [#f (apply (env-thing "itemize") items)]
    ['ordered
     (apply (env-thing "enumerate") items)]
    ['ordered-cont
     (apply (env-thing "enumerate")
            #:options "label={(\\arabic*)}"
            (append (list "\\conti")
                    items
                    (list "\\seti")))]
    ['alpha-ordered
     (apply (env-thing "enumerate") #:options "label=(\\alph*)" items)]
    ['cap-alpha-ordered
     (apply (env-thing "enumerate") #:options "label=(\\Alph*)" items)]))

;; item: faking the scribble 'item'
(define (item . conts)
  (string-append 
   (apply sa "\\item " conts)
   "\n"))



;; given a number of columns and a bunch of items,
;; format them as enumerated items in the specified number
;; of columns
(define (multichoice [cols #f] . items)
  (define real-cols (or cols (length items)))
  ((env-thing "multicols")
   #:arg (number->string real-cols)
   (apply (env-thing "enumerate")
          #:options "label=(\\alph*)"
          (map item items))))

(require rackunit)
(check-equal? (multichoice #f "abcv" "hh.")
              "\\begin{multicols}{2}
\\begin{enumerate}[label=(\\alph*)]
\\item abcv
\\item hh.

\\end{enumerate}

\\end{multicols}
")

;; table... well, enough to get by. Heaven help you if 
;; you have an ampersand or a double-slash in there.
(define (table cols spec lorows)
  (apply (env-thing "tabular") "{"spec"} \\hline\n"
         (append
          (for/list ([row lorows])
            (apply sa (append (add-between row " & ") (list " \\\\\n"))))
          (list "\\hline\n"))))


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
  (apply
   string-append
   (for/list ([p (in-list pkg-strs)])
     (string-append "\\usepackage{" p "}\n"))))

(check-equal? (pkgs '("abc" "def"))
              "\\usepackage{abc}\n\\usepackage{def}\n")

;; given the number of the exam and a string for the current quarter, produce a front page.
;; ultimately, it would be nice to turn TeX macros into racket
;; procedures.
(define (front-page title blurb extra-pkgs package-config-strs key?)
  @sa{\documentclass[11pt]{article}
\usepackage[margin=2.5cm,right=3.5cm]{geometry}
@(pkgs (cons "slatex" extra-pkgs))
@(apply string-append (add-between package-config-strs "\n\n"))

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

@blurb %\hfil\begin{minipage}[t]{4.5cm}

@;{ \begin{tabular}{|c|l|@"@"/r|}
\hline
{\bf Problem} & Points & \\ \hline
1 & & 5\\ \hline
2 & & 10 \\ \hline
3 & & 15 \\ \hline
{\bf Total} & \relax & 30 \\ \hline
\end{tabular}
}
%\end{minipage}

\vfill\thispagestyle{empty}

})

(define listings-package-setup
  #<<|
\lstset{
    aboveskip=0.5em,
    belowskip=0.25em,
    xleftmargin=1.75em,
    numbers=left,
    numberstyle=\scriptsize\ttfamily,
    basicstyle=\normalsize\ttfamily,
    frame=L
}
|
)
