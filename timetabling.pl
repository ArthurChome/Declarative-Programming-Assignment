/*
 DECLARATIVE PROGRAMMING: SECOND ASSIGNMENT
 By Arthur Chomé
*/

/* Import following libraries for the code to work: */
:- use_module(library(lists)).
:- use_module(library(clpfd)).

/* For the arguments, we go from the assumption that 'verb_phrase' gives back */
sentences([Constr1|Constr2]) --> sentence(Constr1), {print(Constr1)}, sentences(Constr2).
sentences([]) --> sentence(Constr1), {print(Constr1)}sentences(Constr2).

/* It's also possible that you have no sentences left. */
sentences([]) --> [].
sentence([A,NP,B]) --> noun_phrase(NP), verb_phrase([A,B]), fullstop, {A\= is, A\= are}.

/* If the verb is empty, the objects contain the time (before or after) for the subject. */
sentence([B,NP,C]) --> noun_phrase(NP), verb_phrase([A,[B|C]]), fullstop.


/* Appending an empty list with a list equals the list that has at least some content. */
noun_phrase([]) --> [].
noun_phrase(C1) --> noun(C1), noun(C2).
noun_phrase(C1) --> noun(C1), [].


/* This way, we can process multiple sentences.*/
/*noun_phrase --> stopFound(X),{print("full stop found"), nl}, sentences.*/

/* Continue parsing the phrase: C1 should be your verb and C2 all classes found for that verb. */
verb_phrase([C1, C2]) --> verb(C1), noun_phrase(C2).

/* Nouns */
/* Prof, students, room, 'and' and class(es) are nouns that we can skip.
 * They are not necessary to find the core subject(s) and object(s) of a sentence.*/
noun(A) --> [prof], noun(A).
noun(A) --> [class], noun(A).
noun(A) --> [classes], noun(A).
noun(A) --> [students], noun(A).
noun(A) --> [room], noun(A).
noun(A) --> [and], noun(A).
noun(A) --> [also], noun(A).
//noun(A) --> [the], noun(A).
//noun(A) --> [same], noun(A).

/* Possible classes for the school: you only have 5 classes.
 * We have to give them numerical values: c1 = 1, c2 = 2, c3 = 3, c4 = 4, a1 = 5 */
noun([1|A]) --> [c1], noun(A).
noun([2|A]) --> [c2], noun(A).
noun([3|A]) --> [c3], noun(A).
noun([4|A]) --> [c4], noun(A).
noun([5|A]) --> [a1], noun(A).

/* Numbers that can be used as subjects or objects
 * Examples: room number, #students that a room can take, etc. */
 noun(100) --> [100], noun(A).
 noun(101) --> [101], noun(A).
 noun(102) --> [102], noun(A).
 noun(35) --> [35], noun(A).
 noun(60) --> [60], noun(A).
 noun(45) --> [45], noun(A).
  noun(1) --> [1], noun(A).

/* Name of the professors
 * Code names: jerry = 1, smith = 2, jones = 3, ruiz = 4, demeuter = 5,
 * hartmann = 6, york = 7, deauxma = 8, signer = 9, frost = 10 */
noun(1) --> [jerry], noun(A).
noun(2) --> [smith], noun(A).
noun(3) --> [jones], noun(A).
noun(4) --> [ruiz], noun(A).
noun(5) --> [demeuter], noun(A).
noun(6) --> [hartmann], noun(A).
noun(7) --> [york], noun(A).
noun(8) --> [deauxma], noun(A).
noun(9) --> [signer], noun(A).
noun(10) --> [frost], noun(A).

/* He and she will have to be processed further with the previously generated Constraints
 * as to know which person (e.g. professor) the sentence is talking about. */
 noun(he) --> [he], noun(A).
 noun(she) --> [she], noun(A).
 noun(sameTeacher) --> [the, same, teacher], noun(A).

/* Time nouns, they will be added to the sentence triple
 * because they represent the predicate's name. */
noun([before|A]) --> [before], noun(A).
noun([after|A]) --> [after], noun(A).
noun([sameDay|A]) --> [on, the, same, day], noun(A).
noun([sameRoom|A]) --> [in, the, same, room], noun(A).
noun([inRoom|A]) --> [in,  room], noun(A).


/* Indicates the end of the sentence hence the end of our subjects/objects list. */
noun([]) --> [].

/* Give back the verb: we have a clause for every verb of the input. */
verb(teaches) --> [teaches].
/* The verb 'is' needs to specify when (after or before) a class takes place.
 * Hence we won't add it to our triple of predicate name, subject and object. */
verb([]) --> [].
verb(is) --> [is].
verb(are) --> [are].
verb(have) --> [have].
verb(has) --> [has].
verb(seats) --> [seats].

teachVerb(verb) --> teachVerb([teaches]).
/* So we can distinguish sentences from each other. */
fullstop --> [fullstop].

/* We need some translation procedures.
 * All courses; professors, etc. are represented with numbers. */
 real_prof(1, jerry).
 real_prof(2, smith).
 real_prof(3, jones).
 real_prof(4, ruiz).
 real_prof(5, demeuter).
 real_prof(6, hartmann).
 real_prof(7, york).
 real_prof(8, deauxma).
 real_prof(9, signer).
 real_prof(10, frost).

 real_class(1, c1).
 real_class(2, c2).
 real_class(3, c3).
 real_class(4, c4).
 real_class(5, a1).

 real_day(1, monday).
 real_day(2, tuesday).
 real_day(3, wednesday).
 real_day(4, thursday).
 real_day(5, friday).


/*
Debugger procedure
It takes 3 arguments because we're working with difference lists:
the original list can be written as 2 lists that need to be subtracted from each other. */

is_verb([teaches]).
is_prof([prof]).


display(X, Y, Z):- write("first part of sentence: "), write(X), nl,
                   write("second part of difference list: "), write(Y), nl,
                   write("third part of sentence: "), write(Z), nl.

newSentence(X, Y, Z):- write("new sentence"), nl.

/* There are some rules for the school of which timetables should get generated: */
no_of_teachers(10).
no_of_classes(5).
studentsClass1(30).
studentsClass2(35).
studentsClass3(100).
studentsClass4(40).
studentsClass5(50).
capacityRoom1(35).
capacityRoom2(60).
capacityRoom3(100).

/* Every class has an amount of students.
 * Remember: c1 = 1, c2 = 2, c3 = 3, c4 = 4 and a1 = 5
 * Can be handy when we're solving constraints. */
has(1, 30).
has(2, 35).
has(3, 100).
has(4, 40).
has(5, 50).

seats(100, 35).
seats(101, 60).
seats(102, 100).


/* Now we need a predicate that adds a list of predicate representations to the database. */
in_same_room(sameRoom).

/* We need some basic list processing since we're using triples of a ver, subject and object.
 * 'findIndex' will find the C'th element of the given list [A|B]. */
 find([Element|List], Element, 1).
 find([Element|List], [Element|List], N) :- N1 = N-1, find(List, Element, N1).

 /* Get first, second and third element of a list. */
 first([E|_], E).
 second([_,E|_], E).
 third([_,_,E|_], E).
 fourth([_,_,_,E|_], E).

/* Difficulties to add the verb to the subject and object.
 * Use predefined verb predicates. */
 verb_teaches(teaches).
 verb_is(is).
 verb_seats(seats).
 verb_are(are).
 verb_have(have).
 verb_has(has).

 /* The 'special' verbes should also be added. */
 is_before(before).
 is_after(after).
 is_same_day(sameDay).
 is_same_room(sameRoom).
 is_in_room(inRoom).

 /* Following predicates process a list of constraints as to replace he and she with the professors. */
 isEqual(A, A).
 isEqual(A, [A|C]).
 isEqual(A, [B|C]):- isEqual(A, C).
 %isEqual(A, [A|B).
 %isEqual(A)
 processConstraints([], A, A):- nl, write("final processed constraint: "), write(A), nl.
 /* Watch the previous constraint if the subject is 'he' or 'she'. */
 processConstraints([A|B], [C|D], Z):- first(A, Verb), second(A, Subj1), third(A, Object), isEqual(Subj1, he),
                                    second(C, Subj2), nl, write("subj2: "), write(Subj2),
                                    processConstraints(B, [[Verb, Subj2, Object], C|D], Z).

/* If the verb is teaches, then we should */

processConstraints([A|B], [C|D], Z):- first(A, Verb), second(A, Subj1), third(A, Object), isEqual(Subj1, she),
                                   second(C, Subj2), nl, write("subj2: "), write(Subj2),
                                   processConstraints(B, [[Verb, Subj2, Object], C|D], Z).

/* If you did not find a 'he' or 'she' in the current constraint, just add it. */
processConstraints([A|B], [C|D], Z):- processConstraints(B, [A,C|D], Z).
processConstraints([A|B], [], Z):- processConstraints(B, [A], Z).

/* Necessary predicates to go through the list of constraints. */
searchConstraints(Verb, Subject, Object, []):- nl, write("not found").
searchConstraints(Verb, Subject, Object, [A|B]):- first(A, FoundVerb),  second(A, FoundSubj), third(A, FoundObj),
                                                 isEqual(Verb, FoundVerb), isEqual(Subject, FoundSubj),
                                                 isEqual(Object, FoundObj), nl, write("subj searched for: "), write(Subject), nl.
searchConstraints(Verb, Subject, Object, [A|B]):- searchConstraints(Verb, Subject, Object, B).

/* Improved version of searchConstraints: this one just gives back the constraint it has found. */
searchConstraints2(Verb, [[Verb|Rest]|Constraints], [Verb|Rest]).

/* Keep iterating untill you found the constraint we were looking for. */
searchConstraints2(Verb, [[A|Rest]|Constraints], Z):- searchConstraints2(Verb, Constraints, Z).

/* The problem: the object here is a list and not an atomic value. */
teaches(A, B, Constraints):- searchConstraints(teaches, A, B, Constraints).
seats(A, B, Constraints):- searchConstraints(seats, A, B, Constraints).
/* Example: does a certain class have a certain amount of students? */
has(A, B, Constraints):- searchConstraints(has, A, B, Constraints).

teaches(6, 1).
teaches(6, 2).
teaches(6, 3).

teaches(2, 1).
teaches(2, 1).
teaches(2, 1).

teaches(3, 1).
teaches(3, 1).
teaches(3, 1).


prof_class_pair(Prof, [], P, P):- write("final result: "), write(P), nl.
prof_class_pair(Prof, [A|B], Pairs, Z):- prof_class_pair(Prof, B, [[Prof, A]|Pairs], Z).

make_class_prof_list([], List, List).
make_class_prof_list([[teaches, Prof, Courses]|Rest], List, Z):- prof_class_pair(Prof, Courses, [], Pairs),
                                                               append(Pairs, List, Newlist), make_class_prof_list(Rest, Newlist, Z).

make_class_prof_list([Something|Rest], List, Z):- make_class_prof_list(Rest, List, Z).

/* Following is a predicate that checks if a list contains a pair of given 2 elements. */
prof_class_exists([[Prof, Class]|Rest], Prof, Class).
prof_class_exists([[A, B]|C], Prof, Class):- prof_class_exists(C, Prof, Class).


/* Are both courses on the same day?
 * The structure of sameday is hard-coded though. */
sameDay(A, B, Constraints):- searchConstraints2(sameDay, Constraints, [SameDay, Days|Nothing]),
                             nl, write("days: "), write(Days), nl,
                             member(A, Days), member(B, Days).

/* Is class A in room B? */
inRoom(A, B, Constraints):- nl, write("yeet"),
                            searchConstraints2(inRoom, Constraints,[InRoom, Classes|Room]),
                            nl, write("in room: "), write(Room), nl,
                            member(A, Classes), isEqual(B, Room).

  /*member([teaches, A, B], Constraints).*/

/* Number association for solving the constraint problem with CLP(FD) */
smith_nr(smith, 1).
jones_nr(jones, 2).

/* Predicate that sees if a given list of pairs contains given two arguments. */
exists([[A, B]|C], A, B). % Base clause: we're done, the list contains a pair of those elements.
exists([[A, B]|C], D, E):- exists(C, D, E).


% Now, we will start labeling our data based on the constraints.
% Our representation for a course
% course(CourseNumber, Class, Room, Day, Start, End).
%:- block course(_,_,_,_,_,_).
constrain_courses([], [], Constraints).

:- block course(-,-,-,-,-,-).


/* If -for given values- there exists no course yet that is un-unifiable, you add the new course with given values. */
constrain_courses([Day, Start, End|Variables], [course(Class, Prof, Room, Day, Start, End)|CourseList], Constraints):-

  /* Make sure the assigned class room is big enough */
  has(Class, NoStudents),
  %teaches(Prof, Class),
  seats(Room, NoSeats),
  NoStudents #=< NoSeats,

  /* There are 5 working days a week (monday, tuesday, etc.)*/
  Day in 1..5, %Should be 5
  Start in 9..14,
  End in 9..14,
  /* Classes only last about an hour. */
  End #= Start + 1,


  /* Next iteration. */
  constrain_courses(Variables, CourseList, Constraints).


  /* Link the courses together. */
  link_courses([]).
  link_courses([_]).

  link_courses([course(Class1, Prof1, Room1, Day1, Start1, End1),
                course(Class2, Prof2, Room2, Day2, Start2, End2)|Courses]):-

      /* Necessary constraint: if not applied, a professor could teach the same class over a day. */
      Prof2 #>= Prof1,
      Start2 #>= Start1,
      Day2 #>= Day1,
    %  Class2 #>= Class1,
      %Room2 #>= Room1,

        (Prof1 #= Prof2 #/\ Day1 #= Day2) #==> \#(End2 #=< Start1 + 2),

       /* If a course starts at the same hour this could mean the following:
        * 1.) It's a different day but the same class 2.) It's the same day but with another professor and room and class. */
       (Start1 #= Start2) #==> #\(Day1 #= Day2) #\/ (  #\(Class1 #= Class2) #/\ #\(Room1 #= Room2) #/\ #\(Prof1 #= Prof2)) ,

       (Class1 #= Class2) #==> #\(Prof1 #= Prof2),
       (Day1 #= Day2 #/\ Start1 #= Start2) #==> #\(Room1 #= Room2),


      %nl, write("current start1: "), write(Start1), nl,
      %nl, write("current start2: "), write(Start2), nl,
      link_courses([course(Class2, Prof2, Room2, Day2, Start2, End2)|Courses]).



/* Final timetable */
timetable(Data, Timetable):-

sentences(Constraints, Data, []),
/* Process the constraints (replace he and she by their real value) */
processConstraints(Constraints, [], P),


length(Timetable, 9),

constrain_courses(Variables, Timetable, P),
link_courses(Timetable),

labeling([ffc], Variables),


/* What's the timetable now? */
write("Time table: "), write(Timetable), nl.

/* Print a list of courses
 * Course(Class, Room, Day, Start, End)  */
 print_course(course(Class, Prof, Room, Day, Start, End)):-

   /* Get the real values for given numbers. */
   real_class(Class, RC), real_prof(Prof, RP), real_day(Day, RD),

   nl, write("Course has following information: "), nl,
   write("Class: "), write(RC), nl,
   write("Room: "), write(Room), nl,
   write("Professor: "), write(RP), nl,
   write("Day: "), write(RD), nl,
   write("Start: "), write(Start), nl,
   write("End: "), write(End), nl.

/* Base clause: when there are no more courses the print, you're good. */
print_courses([]).
print_courses([A|B]):- print_course(A), print_courses(B).

/*Test */
test(1).
test(2).
testAll(L):- findall(X, test(X), L), maplist(test, L).



/* Test Input */
is_test_input([prof, smith, teaches, class, c1, fullstop,
               he, also, teaches, class, c4, fullstop,
               prof, jones, teaches, classes, c1, c2, and, c3, fullstop,
               she, also, teaches, class, c, fullstop,
               class, c1, is, in, room, 1, fullstop,
               classes, c1, c2, and, c3, are, in, the, same, room, fullstop,
               classes, c1, and, c2, have, the, same, teacher, fullstop,
               class, a1, has, 45, students, fullstop,

               /* Specify the capacity of the rooms. */
               room, 100, seats, 35, students, fullstop,
               room, 101, seats, 60, students, fullstop,
               room, 102, seats, 100, students, fullstop,

               class, c1, is, before, class, c2, fullstop,
               class, c4, is, after, class, c3, fullstop,

               /* 3 classes are on the same day. */
               classes, c1, and, c4, are, on, the, same, day, fullstop
               ]).

/* Less complicated test input */
is_simple_test_input([prof, smith, teaches, c3, fullstop,
                      she, also, teaches, class, c2, fullstop,
                      prof, jerry, teaches, c3, fullstop,
                      he, also, teaches, class, c4, fullstop
                      /*prof, jones, teaches, classes, c1, c2, and, c3, fullstop,
                      room, 102, seats, 100, students, fullstop,
                      class, c1, is, before, class, c2, fullstop,
                      classes, c1, c3, and, c4, are, on, the, same, day, fullstop,
                      classes, c1, c2, and, c3, are, in, the, same, room, fullstop,
                      classes, c1, and, c2, have, the, same, teacher, fullstop*/
                      ]).

/* Call the test */
%test :- is_test_input(X), sentences(Constraints, X, []),
        /* Debugger */
%        nl, write("first constraints: "), write(Constraints), nl,
        /* Process them constraints */
%        processConstraints(Constraints, [], ProcessedConstraints),
        /* Debugger */
%        nl, write("final constraints list lalala: "), write(ProcessedConstraints), nl
        /* , teaches(smith, [c1], ProcessedConstraints) */
%        .
test2 :- make_class_prof_list([[teaches,3,[2]], [inRoom,[1],1],[teaches,5,[2, 3, 1]]], [], Final),
         nl, write("Final List: "), write(Final), nl.

test :- timetable([
               prof, smith, teaches, class, a1, c1, c2, c3, c4, fullstop,
               %he, also, teaches, class, c4, fullstop, % If we remove this sentence, everything crashes.
               prof, jones, teaches, classes, a1, c1, c2, c3, c4, fullstop,
               prof, ruiz, teaches, classes, a1, c1, c2, c3, c4, fullstop,
               %he, also, teaches, class, c4, fullstop,
               %she, also, teaches, class, c2, fullstop,
               class, c1, is, in, room, 1, fullstop,
               classes, c1, and, c2, have, the, same, teacher, fullstop,
               %class, a1, has, 45, students, fullstop,

               /* Specify the capacity of the rooms. */
               room, 100, seats, 35, students, fullstop,
               room, 101, seats, 60, students, fullstop,
               room, 102, seats, 100, students, fullstop%,

              % class, c1, is, before, class, c2, fullstop,
               %class, c4, is, after, class, c3, fullstop,
               %classes, c1, c3, and, c4, are, on, the, same, day, fullstop
               ],
               Timetable),
                  nl, write("final timetable: "), print_courses(Timetable).
