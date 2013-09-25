%% Fichier cas_dt.cls à partir du fichier 'cepiiwp09.cls'
%% créé par Benjamin Carton - 27/11/2007
%% Il permet la mise en page automatique des documents de travail du CAS
%% Il n'a pas vocation à circuler. Ce fichier est commenté.
\NeedsTeXFormat{LaTeX2e}[1995/12/01]
\ProvidesClass{cas_dt}[2010/12/03 v1.1 Standard LaTeX class]
\DeclareOption{twocolumn}{\OptionNotUsed} % pas de documents à 2 colonnes
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax
\LoadClass[onecolumn,11pt,a4paper,twoside]{article}

\RequirePackage{fancyhdr,times} % entêtes et pieds de page, police
\RequirePackage{natbib}
\RequirePackage[T1]{fontenc}
\RequirePackage[english,french]{babel} % multilingue
\RequirePackage{color}
\RequirePackage[ansinew]{inputenc}
\RequirePackage{caption}
\RequirePackage{titlesec,titletoc} % format des titres
\RequirePackage{graphicx}
\RequirePackage[marginal]{footmisc}
\RequirePackage[colorlinks=true,urlcolor=black,linkcolor=black,citecolor=black]{hyperref}
\RequirePackage{float}
\RequirePackage{amsmath}
\RequirePackage{tikz}
%\RequirePackage{Sweave}
\RequirePackage{helvet}
\RequirePackage{epstopdf}
\RequirePackage{amsfonts}
\RequirePackage{amsthm}
\RequirePackage{thmtools}

\newenvironment{smatrix}{\begin{pmatrix}}{\end{pmatrix}}

% --------------------------------
% modification de la page de titre
% --------------------------------
\definecolor{cas}{cmyk}{0.75,0.65,0.15,0}
\newcounter{footnotememory}
\def\wpnumber#1{\gdef\@wpnumber{#1}}
\def\frtitle#1{\gdef\@frtitle{#1}}
\def\ltitle#1{\gdef\@ltitle{#1}}
\def\support#1{\gdef\@support{#1}}

\addto\captionsfrench{%
  \renewcommand{\listfigurename}{Liste des graphiques}
  \renewcommand{\listtablename}{Liste des tableaux}
  %\renewcommand{\listtheoremname}{Liste des encadrés}
  \renewcommand{\tablename}{Tableau}
  \renewcommand{\figurename}{Graphique}
}

\renewcommand{\maketitle}
{
  \thispagestyle{empty}
  \begin{titlepage}
  \null
    \begin{tikzpicture}[remember picture,overlay]
    \node [scale=1] at (current page.center) {\includegraphics{first.pdf}};
    \end{tikzpicture}
    \begin{center}
    \begin{minipage}{12cm}
    \begin{flushleft}
    \vspace{5cm}
    {\huge\textbf{\textsf{\@ltitle}}} \\
    \vspace{2cm}
    {\LARGE\textsf{\@author}} \\
    \vspace{2cm}
    {\large\textsf{\@support}} \\
    \vspace{2cm}
    {\large\textsf{n° \@wpnumber, \@date}}
    \end{flushleft}
    \end{minipage}
    \end{center}
    \end{titlepage}
    \clearpage
    %\setcounter{page}{2}%
    \newpage
    \thispagestyle{empty}
    \null
    \newpage
}

% ------------------------
% éléments de mise en page
% ------------------------
%vertical (page)
% tout en haut : 1 inch = 25.4mm + 7mm ajoutés par latex (pourquoi ?)
\setlength{\voffset}{-2.5mm} %positionnement haut de page
\setlength{\headheight}{6mm} % hauteur de l'entête
\setlength{\headsep}{5mm} % hauteur entre entête et haut de texte
\setlength{\textheight}{210mm} % hauteur du texte
\setlength{\footskip}{13mm} % bas de texte / bas de numéro de page
% horizontal (page)
% tout à gauche : 1 inch = 25.4mm
\setlength{\oddsidemargin}{-0.4mm} % marge de gauche des pages impaires
\setlength{\evensidemargin}{-0.4mm} % marge de gauche des pages pairs
\setlength{\textwidth}{160mm} %largeur du texte
% paragraphes
\setlength{\parindent}{0cm} %pas d'alinéa
\setlength{\parskip}{1.8ex plus 0.2ex minus 0.2ex} % espace entre les paragraphes
% notes de bas de page
\setlength{\footnotemargin}{8pt}
% titre de figures te tableaux
\captionsetup{margin=10pt,font={color=cas,normalsize,bf},labelfont={normalsize,bf},labelsep=endash}
\setlength{\abovecaptionskip}{15pt}
\setlength{\belowcaptionskip}{15pt}
% entête
\pagestyle{fancy}
\fancyhead{}
\fancyhead[LE]{\raisebox{4pt}{\footnotesize{\emph{Commissariat Général à la Stratégie et à la Prospective, Document de travail n° \@wpnumber}}}}
\fancyhead[RO]{\raisebox{4pt}{\footnotesize{\emph{\@title}}}}
\fancyfoot[CE,CO]{\small{\thepage}}
\renewcommand{\headrulewidth}{0.4pt}

%----------------------
% Les titres de section
%----------------------
\titleformat{\section}
  {\normalfont\scshape\bfseries\color{cas}}{\thesection.}{1em}{}
\titleformat{\subsection}
  {\normalfont\bfseries}{\thesubsection.}{1em}{}
\titleformat{\subsubsection}%[block]
  {\normalfont\itshape\bfseries}{\thesubsubsection.}{1em}{}
\titlespacing*{\section}      {0pt}{1.75ex plus 1ex minus .2ex}{0.5ex plus .2ex}
\titlespacing*{\subsection}   {0pt}{1.5ex plus 1ex minus .2ex}{0.3ex plus .2ex}
\titlespacing*{\subsubsection}{0pt}{1.25ex plus 1ex minus .2ex}{0.1ex plus .2ex}

% --------------------------
% environnements des résumés
% --------------------------
\newenvironment{longabstract}
    {%
    \phantomsection
    \addcontentsline{toc}{section}{Non-technical summary}
    \selectlanguage{english}
    \null
    {\vskip 3.5ex \@plus 1ex \@minus .2ex}
    \begin{flushright}
    \textbf{\textsc{\@ltitle}}\par
    \end{flushright}
    {\vskip 3.5ex \@plus 1ex \@minus .2ex}%  
    \section*{Non-technical Summary}
    \begin{small}
    }{%
    \end{small}
    \par}
\newenvironment{newabstract}
    {%
    \phantomsection
    \addcontentsline{toc}{section}{Abstract}
    %{\vskip 3.5ex \@plus 1ex \@minus .2ex}%
    \section*{Abstract}
    \begin{small}
    }{%
    \end{small}%
    \par}
\newenvironment{keywords}[1][]
    {%
    \begin{small}
    {\vskip 1ex \@plus 1ex \@minus .2ex}%
    \selectlanguage{english}
    \ifx\\#1\\
    \else
        \makebox[32mm][l]{\emph{JEL Classification}:} #1\par
        \fi
    \makebox[32mm][l]{\emph{Keywords} : }\begin{minipage}[t]{125mm}}{\end{minipage}%
    \end{small}
    \par}
\newenvironment{synthese}
    {%
    \phantomsection
    \addcontentsline{toc}{section}{Synthèse}
    \selectlanguage{french}
%    \null
%    {\vskip 3.5ex \@plus 1ex \@minus .2ex}
%    \begin{flushright}
%    \textbf{\textsc{\@frtitle}}\par
%    \end{flushright}
%    {\vskip 3.5ex \@plus 1ex \@minus .2ex}%  
    \section*{Synthèse}
    \begin{small}
    }{%
    \end{small}
    \par}
 \newenvironment{resume}
    {%
    \phantomsection
    \addcontentsline{toc}{section}{Résumé}
    \section*{Résumé}
    \begin{small}
    }{%
    \end{small}
    \par}
\newenvironment{motsclefs}[1][]
    {%
    \begin{small}
    {\vskip 1ex \@plus 1ex \@minus .2ex}% 
    \selectlanguage{french}
    \ifx\\#1\\
    \else
    \makebox[32mm][l]{\emph{Classification JEL} :} #1\par
    \fi
    \makebox[32mm][l]{\emph{Mots cl\'{e}s} :}\begin{minipage}[t]{125mm}}{\end{minipage}%
    \end{small}
    \par}

% -------------------------
% environnement affiliation
% -------------------------
\newenvironment{affiliation}[1][]
    {%
    \null
    {\vskip 3.5ex \@plus 1ex \@minus .2ex}
    \begin{center}
    \textbf{{\textsc{\textcolor{cas}\@ltitle}}}
        \ifx\\#1\\\par
    \else
    \footnote{#1}\par
    \fi
    \setcounter{footnotememory}{\thefootnote}
    \setcounter{footnote}{0}
    \renewcommand\thefootnote{\@fnsymbol\c@footnote}
    }{%
    \end{center}
    {\vskip 3.5ex}
    \setcounter{footnote}{\thefootnotememory}
    }%

% ---------------------
% environnement de list
% ---------------------
\def\@listI{%
	\setlength{\leftmargin}{15pt}
  \topsep\z@
  \parsep 0.2ex
  \itemsep 0.3ex}
\let\@listi\@listI
\@listi
\def\@listii{\setlength{\leftmargin}{15pt}
   \labelwidth\leftmarginii\advance\labelwidth-\labelsep
   \topsep\z@ \parsep\z@ \itemsep\z@}
\def\@listiii{\setlength{\leftmargin}{15pt}
    \labelwidth\leftmarginiii\advance\labelwidth-\labelsep
    \topsep\z@ \parsep\z@ \itemsep\z@}
\partopsep=\z@

% ---------------------
% La table des matières
% ---------------------
%\setcounter{tocdepth}{2}
%\titlecontents{section}[0pt]{}{\makebox[6mm][l]{\thecontentslabel.}}{}{\titlerule*[1pc]{.}\contentspage}
%\titlecontents{subsection}[1em]{}{\makebox[9mm][l]{\thecontentslabel.}}{}{\titlerule*[1pc]{.}\contentspage}
\renewcommand\tableofcontents{%
    \clearpage
    \phantomsection
    \section*{\hspace{56mm}Table des matières}
    \setlength{\parskip}{0.2ex plus 0.2ex minus 0.2ex}
    \@starttoc{toc}%
    \setlength{\parskip}{1.5ex plus 0.2ex minus 0.2ex}
    %\clearpage
    %\setcounter{page}{2}%
    }

% -----------
% Les annexes
% -----------
\renewcommand\appendix{%
    \clearpage
    \phantomsection
    \addcontentsline{toc}{section}{Annexes}
    \section*{\hspace{0.4\textwidth}Annexes}
  	\addtocounter{section}{1}
  	\setcounter{subsection}{0}
  	\setcounter{table}{0}
  	\setcounter{figure}{0}
  	\gdef\thesubsection{\@Alph\c@subsection}
  	\gdef\thetable{\@Alph\c@subsection.\@arabic\c@table}
  	\gdef\thefigure{\@Alph\c@subsection.\@arabic\c@figure}
  	}

% ----------------
% La bibliographie
% ----------------
\bibliographystyle{apalike-fr}
\renewenvironment{thebibliography}[1]{%
    \clearpage
    \phantomsection
    \section*{\hspace{0.4\textwidth}Bibliographie}
    \addcontentsline{toc}{section}{Bibliographie}
  \list{}%
    {\small
     \setlength\labelwidth{0pt}%
     \setlength\labelsep{0pt}%
     \setlength\leftmargin{25pt}
     \setlength\itemindent{-25pt}
     \setlength\parsep\parskip
%     \@openbib@code
%     \usecounter{enumiv}%
%     \let\p@enumiv\@empty
%     \renewcommand\theenumiv{\@arabic\c@enumiv}
}% 
  \sloppy
  \clubpenalty4000
  \@clubpenalty \clubpenalty
  \widowpenalty4000%
  \sfcode`\.\@m
  }

% ------------------------------------------
% La liste des documents de travail du CAS
% ------------------------------------------
\newcommand\listwp{%
    \clearpage
    \phantomsection
    \thispagestyle{empty}
    %\section*{\hspace{0.4\textwidth}Liste des documents de travail du CAS}
    %\addcontentsline{toc}{section}{Liste des documents de travail du CAS}
    \begin{tikzpicture}[remember picture,overlay]
    \node [scale=1] at (current page.center) {\includegraphics{C:/Users/Utilisateur/Dropbox/cas_dt/last.pdf}};
    \end{tikzpicture}
    }

\endinput
% End of file `cas_dt.cls'.