chapter(chapter1).
chapter(chapter2).
section(section1).
section(section2).
section(section3).

writebookpart(C,S) :- chapter(C), section(S).

