#lang at-exp racket

(require racket/contract
         slatex/slatex-wrapper
         scramble/regexp)

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
         center
         lstlisting
         listings-package-setup
         tikz-cons-tree
         tikz-tree)

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
  [instructions-sheet (->* (string? doc-element? path? string?)
                      (#:extra-pkgs (listof string?)
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

(define (instructions-sheet title blurb file-path file-stem
                            #:extra-pkgs [extra-pkgs '()]
                            #:pkg-config-strs [package-config-strs '()])
  ;; just check to make sure this doesn't signal an error:
  (define target-path (build-path file-path (~a file-stem ".tex")))
  (display-to-file 
   (string-append (front-page title blurb extra-pkgs package-config-strs #f
                              #:name #f)
                  @string-append{\end{document}})
   target-path
   #:exists 'truncate)
  (pdf-slatex (path->string target-path)))

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
                  "\\hrule"
                  "\\begin{centering}

\\small things below this line might not be graded...

\\end{centering}"
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
(make-samename-env-thing centering) ;; usually don't want this...
(make-samename-env-thing center)
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

(module+ test
  (require rackunit)
  (check-equal? (multichoice #f "abcv" "hh.")
                "\\begin{multicols}{2}
\\begin{enumerate}[label=(\\alph*)]
\\item abcv
\\item hh.

\\end{enumerate}

\\end{multicols}
"))

;; table... well, enough to get by. Heaven help you if 
;; you have an ampersand or a double-slash in there.
(define (table _ spec lorows #:lines [lines? #f])
  (apply (env-thing "tabular") "{"spec"} \\hline\n"
         (append
          (for/list ([row lorows])
            (apply sa (append (add-between row " & ") (list " \\\\\n" (if lines? "\\hline \n" "")))))
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

(module+ test
  (check-equal? (pkgs '("abc" "def"))
                "\\usepackage{abc}\n\\usepackage{def}\n"))

;; given the number of the exam and a string for the current quarter, produce a front page.
;; ultimately, it would be nice to turn TeX macros into racket
;; procedures.
(define (front-page title blurb extra-pkgs package-config-strs key?
                    #:name [name? #t])
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

@(if name?
@sa{
\begin{center}
\begin{tabular}@sa|{{l@{\qquad}l}}|
Name:  & \rule{200pt}{.1pt} \\[.5cm]
\end{tabular}
\end{center}}
@sa{})

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


;; drawing binary trees:


(define (line-decl label1 label2)
  (unless (regexp-match (px ^ (+ (chars alpha)) $) label1)
    (error 'line-decl "expected labels consisting of letters, got: ~a\n" label1))
  (unless (regexp-match (px ^ (+ (chars alpha)) $) label2)
    (error 'line-decl "expected labels consisting of letters, got: ~a\n" label2))
  (format "\\draw (~a) to (~a);" label1 label2))

(define (tikz-node-decl type label height x content)
  (unless (regexp-match (px ^ (+ (chars alpha)) $) label)
    (error 'tikz-node-decl "expected label consisting of letters, got: ~a\n" label))
  (unless (exact-integer? height)
    (error 'tikz-node-decl "expected height that is an integer, got: ~a\n" height))
  (unless (real? x)
    (error 'tikz-node-decl "expected x that is a real number, got: ~a\n" x))
  (unless (regexp-match? (px ^ (? "'") (* (chars (union digit alpha))) $) content)
    (error 'tikz-node-decl "expected content matching specified regexp, got: ~e\n"
           content))
  (format "\\node[~a] (~a) at (~a,~a) {~a};" type label (~r #:precision 2 x) height
          content))



(define (cons-decl label height x)
  (tikz-node-decl "cons" label height x ""))

(define (null-decl label height x)
  (tikz-node-decl "null" label height x ""))

(define (numnode-decl label height x num)
  (tikz-node-decl "numleaf" label height x (number->string num)))

(define (symnode-decl label height x symbol)
  (tikz-node-decl "symleaf" label height x (~v symbol)))

;; given a tree of cons, empty, and numbers, return a TikZ string
(define (cons-tree-to-tikz tree)
  (define d (tree-depth tree))
  (apply
   string-append
   (map (λ (s) (string-append s "\n"))
        (cons-tree-to-tikz/height tree d "n" 0 8))))


(define (cons-tree-to-tikz/height tree height label min-x max-x)
  (define mid-x (/ (+ min-x max-x) 2))
  (cond [(pair? tree)
         (define left-label (string-append label "l"))
         (define right-label (string-append label "r"))
         (define mid-x (/ (+ min-x max-x) 2))
         (append
          (list
           (cons-decl label height (/ (+ min-x max-x) 2)))
          (cons-tree-to-tikz/height (car tree) (sub1 height) left-label min-x mid-x)
          (cons-tree-to-tikz/height (cdr tree) (sub1 height) right-label mid-x max-x)
          (list
           (line-decl label left-label)
           (line-decl label right-label)))]
        [(null? tree)
         (list
          (null-decl label height mid-x))]
        [(number? tree)
         (list
          (numnode-decl label height mid-x tree))]
        [(symbol? tree)
         (list
          (symnode-decl label height mid-x tree))]
        [else
         (error 'cons-tree-to-tikz/height
                "expected pair, null, number, or symbol, got: ~e"
                tree)]))

;; height of a tree. atoms are of height 0
(define (tree-depth t)
  (cond [(pair? t) (add1 (max (tree-depth (car t)) (tree-depth (cdr t))))]
        [else 0]))

(define tikz-intro
  (apply string-append
  @list{
   \begin{tikzpicture}[
         null/.style={shape=circle,draw,fill=black,inner sep=2pt},
         cons/.style={shape=circle,draw,fill=black,inner sep=0pt},
         numleaf/.style={},
         symleaf/.style={}
      ]}))

(define tikz-outro
  "\\end{tikzpicture}\n")

(define (tikz-wrap content)
  (string-append tikz-intro content tikz-outro))

(define (tikz-cons-tree cons-tree)
  (tikz-wrap (cons-tree-to-tikz cons-tree)))


;; wow, actually I think this is a much nicer way to do this?

(define a-char (char->integer #\a))

;; a tikz-text is either '_ or a different symbol or a string

;; a tikz-node is either a tikz-text or (cons tikz-text (listof tikz-node))
;; the first one is just for convenience, to shorten e.g. (_ (_) (_)) to (_ _ _)

;; given a name (not shown) and a tikz-node, emit the text necessary
;; to render the tree in tikz. It appears that tikz requires nodes to have unique
;; names, and these are generated by adding characters chosen from the set
;; (a, b, ...) to the end of the existing name. So, if the initial name is na,
;; and it has three children, they would be named naa, nab, and nac.
(define (tikz-tree-node-render name node)
  ;; this enables the abbreviation of a no-children node:
  (define-values (text children)
    (match node
      [(cons text children) (values text children)]
      [(or (? string?) (? symbol?) (? number?)) (values node '())]))
  ;; this enables the abbreviation of " " as '_ and of "abc" as 'abc:
  (define expanded-text
    (match text
      ['_ " "]
      [(? symbol? sym) (symbol->string sym)]
      [(? string? s) s]
      [(? exact-integer? n) (number->string n)]
      [other (error 'btree-node-render "unexpected value for node text: ~e" text)]))
  (when (< 26 (length children))
    (error "naming convention requires fewer than 26 children per node, got ~e"
           children))
  (format "node (~a) {\\texttt{~a}} ~a"
          name
          expanded-text
          (apply
           string-append
           (add-between
            (for/list ([c children]
                       [i (in-naturals)])
              (define sub-name (string-append name
                                              (string
                                               (integer->char (+ a-char i)))))
              (format "child {~a}" (tikz-tree-node-render sub-name c)))
            " "))))

(module+ test
  (check-equal? (tikz-tree-node-render "n" '("x"))
                "node (n) {\\texttt{x}} ")
  (check-equal? (tikz-tree-node-render "n" '("x" ("y") ("z")))
                "node (n) {\\texttt{x}} child {node (na) {\\texttt{y}} } \
child {node (nb) {\\texttt{z}} }"))

(define btree-str
  (apply
   string-append
   @list{
                   \node (n) {\texttt{ }}
                        child {node (na) {\texttt{ }}
                            child {node (n4) {\texttt{ }}
                                child {node (n8) {\texttt{ }}}
                                child {node (n9) {\texttt{ }}}}
                            child {node (n5) {\texttt{ }}
                                child {node (n10) {\texttt{ }}
                                    child {node (n20) {\texttt{ }}
                                        child {node (n40) {\texttt{ }}}
                                        child {node (n41) {\texttt{ }}}}
                                    child {node (n21) {\texttt{ }}}}
                                child {node (n11) {\texttt{ }}}}}
                        child {node (nb) {\texttt{ }}
                            child {node (n6) {\texttt{ }}}
                            child {node (n7) {\texttt{ }}}};
}))

(define btree-example
  (apply
   string-append
@list{
                \begin{tikzpicture}[
                    level distance=1cm, thick, draw=black, text=black,
                    every node/.style={circle, draw=black, minimum width=0.75cm},
                    level 1/.style={sibling distance=8cm},
                    level 2/.style={sibling distance=4cm},
                    level 3/.style={sibling distance=2cm},
                    level 4/.style={sibling distance=1cm},
                    level 5/.style={sibling distance=0.75cm}]

                    @btree-str
                \end{tikzpicture}
}))

(define (tikz-tree btree-sketch)
  (apply
   string-append
@list{
                \begin{tikzpicture}[
                    level distance=1cm, thick, draw=black, text=black,
                    every node/.style={circle, draw=black, minimum width=0.75cm},
                    level 1/.style={sibling distance=8cm},
                    level 2/.style={sibling distance=4cm},
                    level 3/.style={sibling distance=2cm},
                    level 4/.style={sibling distance=1cm},
                    level 5/.style={sibling distance=0.75cm}]

                    \@(tikz-tree-node-render "n" btree-sketch);
                \end{tikzpicture}
}))





(module+ test
  (require rackunit)
  (check-equal? (tree-depth '(4 (2))) 3)

  (check-equal? (cons-tree-to-tikz '(4 (2)))
                #<<|
\node[cons] (n) at (4,3) {};
\node[numleaf] (nl) at (2,2) {4};
\node[cons] (nr) at (6,2) {};
\node[cons] (nrl) at (5,1) {};
\node[numleaf] (nrll) at (4.5,0) {2};
\node[null] (nrlr) at (5.5,0) {};
\draw (nrl) to (nrll);
\draw (nrl) to (nrlr);
\node[null] (nrr) at (7,1) {};
\draw (nr) to (nrl);
\draw (nr) to (nrr);
\draw (n) to (nl);
\draw (n) to (nr);

|
                ))

