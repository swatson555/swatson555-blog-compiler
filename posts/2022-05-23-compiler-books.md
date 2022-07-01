---
img: tiger-book.png
title: Essentials of Compilation & Modern Compiler Implementation in C
abstract: This review goes over two undergraduate textbooks on compilers that have been used autodidactically. The books are shown compared to each other, in the context of their supplementary material, and in of themselves.
---

There are two books that I have considered opinions on. Essentials of Compilation and Modern Compiler Implementation in C (the tiger book). Both books serve as undergraduate texts on compiler implementation. Modern Compiler Implementation in C was published in '97 and, was written to be a replacement to popular older books that didn't have the insight of the significant research that had been published since. Essentials of Compilation was made available recently and, was written for Indiana University's undergraduate compiler course.


# Criteria

There are a few criteria which I'll focus on in this review.

* Pragmatism of content.

* Pedagogical quality.

* Quality of engagement.

* Broader perspective.


# Pragmatism

The tiger book is a very pragmatic book. It presents a language, the tiger language, whose semantics are only slightly larger than C's semantics. The book has [starter code](https://www.cs.princeton.edu/~appel/modern/c/project.html) available on it's [website](https://www.cs.princeton.edu/~appel/modern/c/). It's also well studied and, completed compilers are available on Github. On it's own, just the book, there isn't enough material to complete the chapters in a reasonable amount of time or effort. The book requires more than just the material presented.

Essentials of Compilation presents a subset of Racket for compilation which is also the implementation language of the compiler. The book has a lot of supporting material. There are [video lectures](https://iucompilercourse.github.io/IU-P423-P523-E313-E513-Fall-2020/) and [starter code](https://github.com/IUCompilerCourse/public-student-support-code) available on Github. The book on it's own is enough to study the techniques of the compiler it presents.

Both books present strategies for compilation that generalize well to languages whose semantics are around the size of C and, both books prepare learners to work on larger compilers or, compilers for other languages well. The tiger book in particular being a more pragmatic book in almost every regard.


# Pedagogy

The pedagogical quality of both books are inconsistent. Formal language is introduced in some places but is often replaced with more colloquial terms later. Essentials of Compilation especially has this problem where formal terms for concepts are omitted in totality which is a strange choice considering it's emphasis on language.

Both books introduce a fair amount of vocabulary and concepts however, neither book prepares learners to fully understand research papers in computer science. Neither book goes over virtual machines for example. Both books present compilation in the same way. It presents them as taking an entire expression and translating it to assembly language via a series of sequential passes.


# Engagement

Essentials of Compilation is a very attractive book. It uses and compiles a semantically rich language. Each chapter of the book ends with a working compiler for a new language. It shows the broader perspective of the techniques you're using in this way and, keeps the learner engaged and motivated.

The tiger book, on it's own, doesn't engage it's reader well and, it's starter code has strange expectation from the learner. The language it implements is minimal and, is only of interest to computer scientists. The book doesn't give enough perspective of why one would study a certain concept before or after another. The book requires a lot of supplementary material to be engaging.


# Perspective

These books were meant to be used in undergraduate courses and, serve as supporting material for a first course in compilers. So, how well do these books do this? They're both reasonable at it. They present common techniques of compiling languages well. Especially the tiger book which has a second part going over general concepts of less common compilation techniques. However, they both pedagogically lack perspective of computer science as a whole. They both present a compiler in a single way and, don't give students enough common vocabulary to do or study research on compilers.
