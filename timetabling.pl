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

newSentence(X, Y, Z):- write("new sentence"), nl.

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

 /* Following predicates process a list of constraints as to replace he and she with the professors. */
 isEqual(A, A).
 isEqual(A, [A|C]).
 isEqual(A, [B|C]):- isEqual(A, C).


  /* Following predicate counts how much different courses can be tought in a week */
  countCourses([], No, No).
  countCourses([[teaches, Prof , [Class|OtherClass]|Empty]|Rest], Counter, Z):- length([Class|OtherClass], Len), write("found length: "), write(Len), nl,
                                                                        NewLen is Len + Counter,countCourses(Rest, NewLen, Z).
  countCourses([A|B], Counter, Z):- write("YEEET"), nl, countCourses(B, Counter, Z).


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
searchConstraints(Verb, Subject, Object, []).
searchConstraints(Verb, Subject, Object, [A|B]):- first(A, FoundVerb),  second(A, FoundSubj), third(A, FoundObj),
                                                 isEqual(Verb, FoundVerb), isEqual(Subject, FoundSubj),
                                                 isEqual(Object, FoundObj), nl, write("FOUND: "), write(Subject), nl.
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


/* Make a list of professor-course pairs. */
prof_class_pair(Prof, [], P, P):- write("final result: "), write(P), nl.
prof_class_pair(Prof, [A|B], Pairs, Z):- prof_class_pair(Prof, B, [[Prof, A]|Pairs], Z).

make_class_prof_list([], List, List).
make_class_prof_list([[teaches, Prof, Courses]|Rest], List, Z):- prof_class_pair(Prof, Courses, [], Pairs),
                                                               append(Pairs, List, Newlist), make_class_prof_list(Rest, Newlist, Z).

make_class_prof_list([Something|Rest], List, Z):- make_class_prof_list(Rest, List, Z).


/* The following predicates make a list where a class is added each time it gets tought by a professor */
prof_classes(Prof, [], C, C):- write("final result: "), write(C), nl.
prof_classes(Prof, [Class|OtherClasses], Classes, Z):- prof_class_pair(Prof, OtherClasses, [Class|Classes], Z).

make_class_list([], List, List).
make_class_list([[teaches, Prof, Classes]|Rest], List, Z):- prof_class_pair(Prof, Classes, [], AllClasses),
                                                               append(Classes, List, Newlist), make_class_prof_list(Rest, Newlist, Z).

make_class_list([Something|Rest], List, Z):- make_class_prof_list(Rest, List, Z).


/* See if a given value is in a list. */


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



/* Put the constraints on the courses.
   Arguments:
   - coursenumbers: 10 teachers x 5 classes = 50 numbers
   - variables for each course: C(lass), R(oom), D(ay), S(tart), E(nd).
   - courses: final list that will get returned. */
constrain_courses([], [], [], Constraints).

:- block course(-,-,-,-,-).

/* Constrain the values for the courses. */
constrain_courses([[Prof, Class]|Rest],[Day, Start|Variables], [course(Class, Prof, Room, Day, Start)|CourseList], Constraints):-

  /* There are 5 working days a week (monday, tuesday, etc.)*/
  Day in 1..5,
  Start in 9..14,

  /* Search how much students given class has. */
  %teaches(Prof, Class, Constraints),
  has(Class, NoStudents),
  seats(Room, NoSeats),
  NoStudents #=< NoSeats,

  /* Next iteration. */
  constrain_courses(Rest, Variables, CourseList, Constraints).


  /* Link the courses together. */
  link_courses([]).
  link_courses([_]).

  compare_all(course(Class, Prof, Room, Day, Start), []).

  compare_all(course(Class1, Prof1, Room1, Day1, Start1),
               [course(Class2, Prof2, Room2, Day2, Start2)|Courses]):-
                %Prof2 #>= Prof1,
                (Prof1 #= Prof2 #/\ Day1 #= Day2) #==> (Start1 #>= Start2 + 2),
                (Day1 #= Day2 #/\ Room1 #= Room2) #==> #\(Start1 #= Start2),
                (Day1 #= Day2 #/\ Room1 #= Room2) #==> #\(Start1 #= Start2),
                (Room1 #= Room2 #/\ Start1 #= Start2) #==> #\(Day1 #= Day2),
                (Class1 #= Class2) #==> #\(Prof1 #= Prof2),
                nl, write("current start1: "), write(Start1), nl,
                nl, write("current start2: "), write(Start2), nl,
                %link_courses(Courses),
                compare_all(course(Class1, Prof1, Room1, Day1, Start1), Courses).



  link_courses([course(Class1, Prof1, Room1, Day1, Start1),
                course(Class2, Prof2, Room2, Day2, Start2)|Courses]):-

      compare_all(course(Class1, Prof1, Room1, Day1, Start1), [course(Class2, Prof2, Room2, Day2, Start2)|Courses]),

      %Prof2 #>= Prof1,
      %(Prof1 #= Prof2 #/\ Day1 #= Day2) #==> (Start1 #>= Start2 + 2),
      %(Day1 #= Day2 #/\ Room1 #= Room2) #==> #\(Start1 #= Start2),

      %nl, write("current start1: "), write(Start1), nl,
      %nl, write("current start2: "), write(Start2), nl,
      %link_courses(Courses),
      link_courses([course(Class2, Prof2, Room2, Day2, Start2)|Courses]).



/* Final timetable */
timetable(Data, Timetable):-

  sentences(Constraints, Data, []),

  /* Process the constraints (replace he and she by their real value) */
  processConstraints(Constraints, [], P),

  /* Number of possible courses */
  make_class_list(Constraints, [], Classes),

  /* It's actually a list of prof-class paris*/
  make_class_prof_list(Constraints, [], Pairs),

  /* The idea comes from the resource allocation from Chapter 15.
   * In it, the predicate 'constrain_boxes' takes in a list of boxnumbers
   * to uniquely identify the boxes.
   * To uniquely identify a course: we need its professor and class (pairs). */
  constrain_courses(Pairs, Variables, Timetable, P),
  nl, write("Constrain timetable: "), write(Timetable), nl,
  link_courses(Timetable),

  labeling([ffc], Variables),

/* What's the timetable now? */
write("Time table: "), write(Timetable), nl
.

/* Print a list of courses
 * Course(Class, Room, Day, Start, End)  */
 print_course(course(Class, Prof, Room, Day, Start)):-

   /* Get the real values for given numbers. */
   real_class(Class, RC), real_prof(Prof, RP), real_day(Day, RD),
   End is Start + 1,
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

/* TIME TABLE PRINTER */
print_header:- nl, write("  _________________________________________________________"), nl,
               print("|      | monday | tuesday | wednesday | thursday | friday |"), nl.


/* Specify a day of which you want courses. */
collectDayCourses([], Day, Courses, Courses).
/* Remember an integer = 1 means monday */
collectDayCourses([course(Class, Prof, Room, Day, Start)|Rest], Day, Courses, Z):-
                      collectDayCourses(Rest, Day, [course(Class, Prof, Room, Day, Start)|Courses], Z).

collectDayCourses([Course|Rest], Day, Courses, Z):-
                      collectDayCourses(Rest, Day, Courses, Z).

/* Specify an hour of which you want courses. */
collectHourCourses([], Hour, Courses, Courses).
/* Remember an integer = 1 means monday */
collectHourCourses([course(Class, Prof, Room, Day, Hour)|Rest], Hour, Courses, Z):-
                      collectHourCourses(Rest, Hour, [course(Class, Prof, Room, Day, Hour)|Courses], Z).

collectHourCourses([Course|Rest], Hour, Courses, Z):-
                      collectHourCourses(Rest, Hour, Courses, Z).

/* Following predicate will print out a course */
printCourse(course(Class, Prof, Room, Day, Hour)):-
  write("Class: "), write(Class), write(", "), write("Prof: "),
  write(Prof), write(", "), write("Room: "), write(Room).

/* Following predicate will print out a line of courses for the whole week for an hour. */
printCoursesWeekHourLine(Monday, Tuesday, Wednesday, Thursday, Friday):-
  print_course(Monday), write(" | "), print_course(Tuesday), print_course(Wednesday),
  write(" | "), print_course(Thursday), write(" | "), print_course(Friday), write(" | "), nl.

printCoursesWeekHour([], [], [], [], []):- nl, write("we're done yeet."), nl.
printCoursesWeekHour([Monday|MondayCourses], [Tuesday|TuesdayCourses], [Wednesday|WednesdayCourses], [Thursday|ThursdayCourses], [Friday|FridayCourses]):-
printCoursesWeekHourLine(Monday, Tuesday, Wednesday, Thursday, Friday),
printCoursesWeekHour(MondayCourses, TuesdayCourses, WednesdayCourses, ThursdayCourses, FridayCourses).







print_table(Timetable):-

  /* Print the found courses. */
  print_courses(Timetable),

  /* Find the courses for each day. */
  collectDayCourses(Timetable, 1, [], MondayCourses),
  collectDayCourses(Timetable, 2, [], TuesdayCourses),
  collectDayCourses(Timetable, 3, [], WednesdayCourses),
  collectDayCourses(Timetable, 4, [], ThursdayCourses),
  collectDayCourses(Timetable, 5, [], FridayCourses),

  /* Select the courses for monday */
  collectHourCourses(MondayCourses, 9, [], MHour9Course),
  collectHourCourses(MondayCourses, 10, [], MHour10Course),
  collectHourCourses(MondayCourses, 11, [], MHour11Course),
  collectHourCourses(MondayCourses, 12, [], MHour12Course),
  collectHourCourses(MondayCourses, 13, [], MHour13Course),
  collectHourCourses(MondayCourses, 14, [], MHour14Course),

  /* Select the courses for tuesday */
  collectHourCourses(TuesdayCourses, 9, [], THour9Course),
  collectHourCourses(TuesdayCourses, 10, [], THour10Course),
  collectHourCourses(TuesdayCourses, 11, [], THour11Course),
  collectHourCourses(TuesdayCourses, 12, [], THour12Course),
  collectHourCourses(TuesdayCourses, 13, [], THour13Course),
  collectHourCourses(TuesdayCourses, 14, [], THour14Course),

  /* Select the courses for wednesday */
  collectHourCourses(WednesdayCourses, 9, [], WHour9Course),
  collectHourCourses(WednesdayCourses, 10, [], WHour10Course),
  collectHourCourses(WednesdayCourses, 11, [], WHour11Course),
  collectHourCourses(WednesdayCourses, 12, [], WHour12Course),
  collectHourCourses(WednesdayCourses, 13, [], WHour13Course),
  collectHourCourses(WednesdayCourses, 14, [], WHour14Course),

  /* Select the courses for monday */
  collectHourCourses(ThursdayCourses, 9, [], THHour9Course),
  collectHourCourses(ThursdayCourses, 10, [], THHour10Course),
  collectHourCourses(ThursdayCourses, 11, [], THHour11Course),
  collectHourCourses(ThursdayCourses, 12, [], THHour12Course),
  collectHourCourses(ThursdayCourses, 13, [], THHour13Course),
  collectHourCourses(ThursdayCourses, 14, [], THHour14Course),

  /* Select the courses for monday */
  collectHourCourses(FridayCourses, 9, [], FHour9Course),
  collectHourCourses(FridayCourses, 10, [], FHour10Course),
  collectHourCourses(FridayCourses, 11, [], FHour11Course),
  collectHourCourses(FridayCourses, 12, [], FHour12Course),
  collectHourCourses(FridayCourses, 13, [], FHour13Course),
  collectHourCourses(FridayCourses, 14, [], FHour14Course),

  /*Print the courses */
  print_header,
    nl, write("Monday courses: "), write(MondayCourses), nl,
  nl, write("Monday hour 9 courses: "), write(MHour9Course), nl,
  %printCoursesWeekHour(MHour9Course, THour9Course, WHour9Course, THHour9COurse, FHour9Course),

  nl, write("Course on monday 9 o'clock: "), write(MHour9Course), nl.



/* TESTS */
/* First Test Input */
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

test :-
    timetable([
               prof, jerry, teaches, class, a1, c1, c2, c3, c4, fullstop,
               prof, smith, teaches, class, a1, c1, c2, c3, c4, fullstop,
               %prof, jones, teaches, class, a1, c1, c2, c3, c4, fullstop,
               %prof, ruiz, teaches, class, a1, c1, c2, c3, c4, fullstop,
               %prof, demeuter, teaches, class, a1, c1, c2, c3, c4, fullstop,

               %prof, hartmann, teaches, class, a1, c1, c2, c3, c4, fullstop,
               %prof, york, teaches, class, a1, c1, c2, c3, c4, fullstop,
               %prof, deauxma, teaches, class, a1, c1, c2, c3, c4, fullstop,
               %prof, signer, teaches, class, a1, c1, c2, c3, c4, fullstop,
               %prof, frost, teaches, class, a1, c1, c2, c3, c4, fullstop,

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
               Timetable), print_table(Timetable).

test2 :- print_header.
