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
 * Code names: jerry = 1, smith = 2, jones = 3, lopez = 4, jager = 5,
 * sevic = 6, chome = 7, acker = 8, stark = 9, frost = 10 */
noun(1) --> [jerry], noun(A).
noun(2) --> [smith], noun(A).
noun(3) --> [jones], noun(A).
noun(4) --> [lopez], noun(A).
noun(5) --> [jager], noun(A).
noun(6) --> [sevic], noun(A).
noun(7) --> [chome], noun(A).
noun(8) --> [acker], noun(A).
noun(9) --> [stark], noun(A).
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
 real_prof(4, lopez).
 real_prof(5, jager).
 real_prof(6, sevic).
 real_prof(7, chome).
 real_prof(8, acker).
 real_prof(9, stark).
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
inRoom(A, B, Constraints):- searchConstraints2(inRoom, Constraints,[InRoom, Classes|Room]),
                            member(A, Classes), isEqual(B, Room).

/* Is class A in room B? */
sameTeacher(A, B, Constraints):-
                             searchConstraints2(have, Constraints, [Have, Classes|sameTeacher]),
                             nl, write("days: "), write(Days), nl,
                             member(A, Days), member(B, Days).

/* Put the constraints on the courses.
   Arguments:
   - coursenumbers: 10 teachers x 5 classes = 50 numbers
   - variables for each course: C(lass), R(oom), D(ay), S(tart), E(nd).
   - courses: final list that will get returned. */
constrain_courses([], [], []).

:- block course(-,-,-,-,-).

/* Constrain the values for the courses. */
constrain_courses([[Prof, Class]|Rest],[Day, Start|Variables], [course(Class, Prof, Room, Day, Start)|CourseList]):-

  /* There are 5 working days a week (monday, tuesday, etc.)*/
  Day in 1..5,
  Start in 9..14,

  /* Search how much students given class has. */
  has(Class, NoStudents),
  seats(Room, NoSeats),
  NoStudents #=< NoSeats,

  /* Next iteration. */
  constrain_courses(Rest, Variables, CourseList).


  /* Link the courses together. */
  link_courses([], Constrtaints).
  link_courses([_], Constraints).

  compare_all(course(Class, Prof, Room, Day, Start), [], Constraints).

  compare_all(course(Class1, Prof1, Room1, Day1, Start1),
               [course(Class2, Prof2, Room2, Day2, Start2)|Courses], Constraints):-

                /* Apply other class specific constraints */
                inRoom(Class,Room, Constraints),
                %(sameDay(Class1, Class, Constraints)) #==> (Day1 #= Day2),

                /* Compare courses */
                (Prof1 #= Prof2 #/\ Day1 #= Day2) #==> (Start1 #>= Start2 + 2),
                (Day1 #= Day2 #/\ Room1 #= Room2) #==> #\(Start1 #= Start2),
                (Day1 #= Day2 #/\ Room1 #= Room2) #==> #\(Start1 #= Start2),
                (Room1 #= Room2 #/\ Start1 #= Start2) #==> #\(Day1 #= Day2),
                (Class1 #= Class2) #==> #\(Prof1 #= Prof2),

                compare_all(course(Class1, Prof1, Room1, Day1, Start1), Courses, Constraints).

  /* Check the constraints for the relations between classes. */
  link_courses([course(Class1, Prof1, Room1, Day1, Start1),
                course(Class2, Prof2, Room2, Day2, Start2)|Courses], Constraints):-

      /* Compare the first course with all other courses. */
      compare_all(course(Class1, Prof1, Room1, Day1, Start1), [course(Class2, Prof2, Room2, Day2, Start2)|Courses], Constraints),

      link_courses([course(Class2, Prof2, Room2, Day2, Start2)|Courses], Constraints).



/* Final timetable */
timetable(Data, Timetable):-

  sentences(Constraints, Data, []),

  /* Process the constraints (replace he and she by their real value) */
  processConstraints(Constraints, [], P),

  /* Debugger */
  nl, write("Processed constraints: "), write(P), nl,

  /* Number of possible courses */
  make_class_list(Constraints, [], Classes),

  /* It's actually a list of prof-class paris*/
  make_class_prof_list(Constraints, [], Pairs),

  /* The idea comes from the resource allocation from Chapter 15.
   * In it, the predicate 'constrain_boxes' takes in a list of boxnumbers
   * to uniquely identify the boxes.
   * To uniquely identify a course: we need its professor and class (pairs). */
  constrain_courses(Pairs, Variables, Timetable),
  nl, write("Constrain timetable: "), write(Timetable), nl,
  link_courses(Timetable, P),

  labeling([ffc], Variables),

/* What's the timetable now? */
write("Time table: "), write(Timetable), nl.

/* TIME TABLE PRINTER */

/* Header for the timetable */
print_header:- nl, write("  -------------------------------------------------------------------------------------------------------------------------------------------------------"), nl,
               print("|       |           DAY 1           |           DAY 2            |           DAY 3            |           DAY 4            |           DAY 5            |"), nl.



/* Find a course at a certain day, hour and room. */
findCourse([], Day, Hour, Room, []).
findCourse([course(Class, Prof, Room, Day, Hour)|Rest], Day, Hour, Room, course(Class, Prof, Room, Day, Hour)).
findCourse([course(Class, Prof, Room, Day, Hour)|Rest], OtherDay, OtherHour, OtherRoom, Z):-
findCourse(Rest, OtherDay, OtherHour, OtherRoom, Z).

  /* Following predicate will print out a course */
  printCourse([]):- write("                          "). % If the course is empty, print nothing.
  printCourse(course(Class, Prof, Room, Day, Hour)):-
    write("Cl: "), real_class(Class, RealC), write(RealC), write(", "), write("Pr: "),
    real_prof(Prof, RealProf), write(RealProf), write(", "), write("Ro: "), write(Room).

  /* Correct the placement of an hour in the timetable: if it's smaller than 10, it needs an additional white space. */
  correct_hour(H):- H < 10, write(" "), write(H).
  correct_hour(H):- write(H).

  print_table_hour(Timetable, 15).
  print_table_hour(Timetable, Hour):-
    /*Print for a specific hour for every week day. */
    write("  ----------------------------------------------------------------------------------------------------------------------------------------------------"), nl,

    write(" |       |"), findCourse(Timetable, 1, Hour, 100, C1), printCourse(C1), write(" | "),
    findCourse(Timetable, 2, Hour, 100, C2), printCourse(C2), write(" | "), findCourse(Timetable, 3, Hour, 100, C3),
    printCourse(C3), write(" | "),
    findCourse(Timetable, 4, Hour, 100, C4), printCourse(C4), write(" | "),
    findCourse(Timetable, 5, Hour, 100, C5), printCourse(C5), write(" | "), nl,

    write(" | "), correct_hour(Hour),write(":00 |"), findCourse(Timetable, 1, Hour, 101, C6), printCourse(C6), write(" | "),
    findCourse(Timetable, 2, Hour, 101, C7), printCourse(C7), write(" | "), findCourse(Timetable, 3, Hour, 101, C8),
    printCourse(C8), write(" | "), findCourse(Timetable, 4, Hour, 101, C9), printCourse(C9), write(" | "),
    findCourse(Timetable, 5, Hour, 101, C10), printCourse(C10), write(" | "), nl,

    write(" |       |"), findCourse(Timetable, 1, Hour, 102, C11), printCourse(C11), write(" | "),
    findCourse(Timetable, 2, Hour, 102, C12), printCourse(C12), write(" | "), findCourse(Timetable, 3, Hour, 102, C13),
    printCourse(C13), write(" | "),
    findCourse(Timetable, 4, Hour, 102, C14), printCourse(C14), write(" | "),
    findCourse(Timetable, 5, Hour, 102, C15), printCourse(C15), write(" | "), nl,

    /* Print the next hour. */
    NextHour is Hour + 1,
    print_table_hour(Timetable, NextHour).


      print_table(Timetable):-
        print_header,
        print_table_hour(Timetable, 9),
        write("  ----------------------------------------------------------------------------------------------------------------------------------------------------"), nl,

        write("  LEGEND"), nl, write("  Cl: class, Pr: prof, Ro: room ").


/* TESTS */

test([
           prof, jerry, teaches, class, a1, c1, c2, c3, c4, fullstop,
           prof, smith, teaches, class, a1, c1, c2, c3, c4, fullstop,
           %prof, jones, teaches, class, a1, c1, c2, c3, c4, fullstop,
           %prof, lopez, teaches, class, a1, c1, c2, c3, c4, fullstop,
           %prof, jager, teaches, class, a1, c1, c2, c3, c4, fullstop,

           %prof, sevic, teaches, class, a1, c1, c2, c3, c4, fullstop,
           %prof, stark, teaches, class, a1, c1, c2, c3, c4, fullstop,
           %prof, chome, teaches, class, a1, c1, c2, c3, c4, fullstop,
           %prof, acker, teaches, class, a1, c1, c2, c3, c4, fullstop,
           %prof, frost, teaches, class, a1, c1, c2, c3, c4, fullstop,

           class, c1, is, in, room, 100, fullstop,
           classes, c1, and, c2, have, the, same, teacher, fullstop,
           %class, a1, has, 45, students, fullstop,

           /* Specify the capacity of the rooms. */
           room, 100, seats, 35, students, fullstop,
           room, 101, seats, 60, students, fullstop,
           room, 102, seats, 100, students, fullstop,

           /* Specifications for all classes. */
           class, c1, is, before, class, c2, fullstop,
           class, c4, is, after, class, c3, fullstop,
           classes, c1, c3, and, c4, are, on, the, same, day, fullstop
           ]).

solution :- test(Data), timetable(Data, Timetable), print_table(Timetable).
