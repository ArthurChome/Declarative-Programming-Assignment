%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                               %
%      DECLARATIVE PROGRAMMING:TIMETABLING      %
%                By Arthur Chomé                %
%                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
* OVERVIEW
* To increase readibility, the assignment has following sections:
* 1. Definite Clause Grammar
* 2. Finite Domains equation solver
* 3. Printing the results
* 4. Tests
*/

/* Import following libraries for the code to work: */
:- use_module(library(lists)).
:- use_module(library(clpfd)).

/*
* 1. DEFINITE CLAUSE GRAMMAR
* Before the constraint solver can start working,
* our DCG parser must convert the sentences of strings into meaningful representations.
*
* The general workflow for the parser is as follows:
* 1. Detect the different sentences of the input by the keyword 'fullstop'.
* 2. Split a sentence into two parts: a noun_phrase containing the subject(s)
*    and a verb_phrase containing the sentence's verb and object(s).
* 3. When backtracking, pass the subject of the noun_phrase and the verb
*    and object of the verb_phrase.
*    They will form a constraint representation triple of the verb, subject(s) and object(s)
*/

/* For the arguments, we go from the assumption that 'verb_phrase' gives back parsed data */
sentences([Constr1|Constr2]) --> sentence(Constr1), sentences(Constr2).
sentences([]) --> sentence(_), sentences(_).

/* It's also possible that you have no sentences left.
 * If it's the case, you're ready. */
sentences([]) --> [].
sentence([A,NP,B]) --> noun_phrase(NP), verb_phrase([A,B]), fullstop, {A\= is, A\= are}.

/* If the verb is empty, the objects contain the time (before or after) for the subject. */
sentence([B,NP,C]) --> noun_phrase(NP), verb_phrase([_,[B|C]]), fullstop.

/* Appending an empty list with a list equals the list that has at least some content. */
noun_phrase([]) --> [].
noun_phrase(C1) --> noun(C1), noun(_).
noun_phrase(C1) --> noun(C1), [].

/* Continue parsing the phrase: C1 should be your verb and C2 all classes found for that verb. */
verb_phrase([C1, C2]) --> verb(C1), noun_phrase(C2).

/* NOUNS
 * Prof, students, room, 'and' and class(es) are nouns that we can skip.
 * They are not necessary to find the core subject(s) and object(s) of a sentence
 * and hence won't be passed back when backtracking using the arguments.*/

noun(A) --> [prof], noun(A).
noun(A) --> [class], noun(A).
noun(A) --> [classes], noun(A).
noun(A) --> [students], noun(A).
noun(A) --> [room], noun(A).
noun(A) --> [and], noun(A).
noun(A) --> [also], noun(A).
noun(A) --> [the], noun(A).
noun(A) --> [same], noun(A).

/* Possible classes for the school: you only have 5.
 * They can form subjects or objects within a sentence and are passed back.
 * We have to give them numerical values: c1 = 1, c2 = 2, c3 = 3, c4 = 4, a1 = 5
 * as to solve the constraint problem. */
noun([1|A]) --> [c1], noun(A).
noun([2|A]) --> [c2], noun(A).
noun([3|A]) --> [c3], noun(A).
noun([4|A]) --> [c4], noun(A).
noun([5|A]) --> [a1], noun(A).

/* Numbers that can be used as subjects or objects
 * They are passed back with backtracking.
 * Examples: room number, #students that a room can take, etc. */
 noun(100) --> [100], noun(_).
 noun(101) --> [101], noun(_).
 noun(102) --> [102], noun(_).
 noun(60) --> [60], noun(_).
 noun(50) --> [50], noun(_).
 noun(45) --> [45], noun(_).
 noun(40) --> [40], noun(_).
 noun(35) --> [35], noun(_).
 noun(30) --> [30], noun(_).
 noun(1) --> [1], noun(_).

/* Name of the professors: they can form subjects or objects.
 * They get translated into numbers for the solver to process.
 * Code names: jerry = 1, smith = 2, jones = 3, lopez = 4, jager = 5,
 * sevic = 6, chome = 7, acker = 8, stark = 9, frost = 10 */
noun(1) --> [jerry], noun(_).
noun(2) --> [smith], noun(_).
noun(3) --> [jones], noun(_).
noun(4) --> [lopez], noun(_).
noun(5) --> [jager], noun(_).
noun(6) --> [sevic], noun(_).
noun(7) --> [chome], noun(_).
noun(8) --> [acker], noun(_).
noun(9) --> [stark], noun(_).
noun(10) --> [frost], noun(_).

/* He and she will have to be processed further with the previously generated Constraints
 * as to know which person (e.g. professor) the sentence is talking about.
 * The predicate doing this is 'processConstraints'... */
 noun(he) --> [he], noun(_).
 noun(she) --> [she], noun(_).
 noun(sameTeacher) --> [the, same, teacher], noun(_).

/* Time nouns, they will be added to the a constraint triple as a 'verb'
 * For some, 4 words will be converted into one verb (sameRoom, etc.) */
noun([before|A]) --> [before], noun(A).
noun([after|A]) --> [after], noun(A).
noun([sameDay|A]) --> [on, the, same, day], noun(A).
noun([sameRoom|A]) --> [in, the, same, room], noun(A).
noun([inRoom|A]) --> [in, room], noun(A).


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

/* We need some translation procedures to convert strings into numbers
 * to apply them on the constrain problem.
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

 /*
 * 2. FINITE DOMAINS EQUATION SOLVER
 * Given the parsed sentences with their own meaningful representation,
 * the solver will now generate possible timetables with them
 * and possibly give back 'false' if they are too harsh to respect.
 */


/* Every class has an amount of students.
 * Remember: c1 = 1, c2 = 2, c3 = 3, c4 = 4 and a1 = 5
 * Can be handy when we're solving constraints. */
/* Does a given class have a given number of students? */
has(Class, NoStudents, Constraints):-
  searchConstraints(Constraints, has, [], Pairs),
  searchPairs(Class, NoStudents, Pairs).

/* Does a given class have a given number of students? */
seats(Room, NoStudents, Constraints):-
  searchConstraints(Constraints, seats, [], Pairs),
  write("seats pairs: "), write(Pairs), nl,
  searchPairs(Room, NoStudents, Pairs).

seats(100, 35).
seats(101, 60).
seats(102, 100).

/* BASIC LIST PROCESSING AND EQUALITY ALGORITHMS
 * The following are some list processing predicates as to use and manipulate
 * the list of generated constraint representations from the parser.
 */

/* We need some basic list processing since we're using triples of a ver, subject and object.
 * 'findIndex' will find the C'th element of the given list [A|B]. */
 find([Element|_], Element).
 find([_|List], OtherElement) :- find(List, OtherElement).
 find(Element, Element).

 /* Get first, second and third element of a list. */
 first([E|_], E).
 second([_,E|_], E).
 third([_,_,E|_], E).
 fourth([_,_,_,E|_], E).

 /* Following predicates process a list of constraints as to replace he and she with the professors. */
 isEqual(A, A).
 isEqual(A, [A|_]).
 isEqual(A, [_|C]):- isEqual(A, C).

 processConstraints([], A, A).
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

/* Following predicate make a list of classes-room pairs. */
/* Improved version of searchConstraints: this one just gives back the constraint it has found. */
searchInRooms([], Pairs, Pairs).
searchInRooms([A|B], Pairs, Z):- first(A, FoundVerb), second(A, Classes), third(A, Room),
                                       isEqual(inRoom, FoundVerb), searchInRooms(B, [[Classes, Room]|Pairs], Z).
searchInRooms([_|B], Pairs, Z):- searchInRooms(B, Pairs, Z).

/* Generic procedure that makes a list of pairs of the second and third elements
 * of triples in the list that match with given verb.*/
 searchConstraints([], _, Pairs, Pairs).
 searchConstraints([A|B], Verb, Pairs, Z):- first(A, FoundVerb), second(A, Second), third(A, Third),
                                             %write("found: "), write([Second, Third])
                                             isEqual(Verb, FoundVerb), searchConstraints(B, Verb, [[Second, Third]|Pairs], Z).
searchConstraints([_|B], Verb, Pairs, Z):- searchConstraints(B, Verb, Pairs, Z).


searchSameDay([], _, Pairs, Pairs).
searchSameDay([A|B], Verb, Pairs, Z):- first(A, FoundVerb), second(A, Second), third(A, _),
                                       isEqual(Verb, FoundVerb), searchSameDay(B, Verb, [Second|Pairs], Z).
searchSameDay([_|B], Verb, Pairs, Z):- searchSameDay(B, Verb, Pairs, Z).


/* See if a given class is before another class. */

/* This predicate searches a list of pairs with every element within a list of classes
 * for the two given classes. */
searchPairs(Class1, Class2, [[A, B]|_]):- %write("found pairs: "), write(A), write(" and "), write(B),
                                                   find(A, Class1), find(B, Class2).
%searchPairs(Class1, Class2, [[A, B]|OtherPairs]):- find(A, Class2), find(B, Class1).
searchPairs(Class1, Class2, [[_, _]|OtherPairs]):- searchPairs(Class1, Class2, OtherPairs).

/* For the same day predicate, the third element of the triple is an empty list.
 * This is why we must search the two classes in the second element. */
searchDay(Class1, Class2, [A|_]):- find(A, Class1), find(A, Class2).

searchDay(Class1, Class2, [_|OtherPairs]):- searchDay(Class1, Class2, OtherPairs).

/* Give back the list of classes that are on the same day and contain given two classes. */
sameDayClasses(_, _, [], []).
sameDayClasses(Class1, Class2, [[A, _]|_], A):- find(A, Class1), find(A, Class2).
sameDayClasses(Class1, Class2, [[_, _]|OtherPairs], Z):- sameDayClasses(Class1, Class2, OtherPairs, Z).


/* Process the list of class pairs given a class */
findRoom([[Classes, Room]|_], Class, Room):- find(Classes, Class).
findRoom([[_, Room]|OtherPairs], Class, Room):- findRoom(OtherPairs, Class, Room).

/* Make a list of professor-course pairs. */
prof_class_pair(_, [], P, P).
prof_class_pair(Prof, [A|B], Pairs, Z):- prof_class_pair(Prof, B, [[Prof, A]|Pairs], Z).

make_class_prof_list([], List, List).
make_class_prof_list([[teaches, Prof, Courses]|Rest], List, Z):- prof_class_pair(Prof, Courses, [], Pairs),
                                                               append(Pairs, List, Newlist), make_class_prof_list(Rest, Newlist, Z).

make_class_prof_list([_|Rest], List, Z):- make_class_prof_list(Rest, List, Z).

/* Second prototype for inRoom: will take an original room
 * and see if it can find a constraint that wants the given class to be in a specific room */
inRoom(Constraints, Class, Room):- searchConstraints(Constraints, inRoom, [], Pairs), findRoom(Pairs, Class, AnswerRoom),
                                   \+ isEqual(AnswerRoom, []), Room #= AnswerRoom.
inRoom(_, _, _).


before(course(_, Class1, _, _, Start1), course(_, Class2, _, _, Start2), Constraints):-
  searchConstraints(Constraints, before, [], Pairs),
  searchPairs(Class1, Class2, Pairs), !,
   Start1 #< Start2.

before(course(_, _, _, _, _), course(_, _, _, _, _), _).

/* Constraint that some classes need to take place after some other classes. */
after(course(_, Class1, _, _, Start1), course(_, Class2, _, _, Start2), Constraints):-
  searchConstraints(Constraints, after, [], Pairs),


  searchPairs(Class1, Class2, Pairs), !,
  Start1 #> Start2.

after(course(_, _, _, _, _), course(_, _, _, _, _), _).

/* For same day to work, there should be not too much classes for a day. */
sameDay(course(_, Class1, _, Day1, _), course(_, Class2, _, Day2, _), Constraints):-
  searchSameDay(Constraints, sameDay, [], Pairs),
  searchDay(Class1, Class2, Pairs), !,
  Day1 #= Day2.

sameDay(course(_, _, _, _, _), course(_, _, _, _, _), _).



/* Put the constraints on the courses.
   Arguments:
   - coursenumbers: 10 teachers x 5 classes = 50 numbers
   - variables for each course: C(lass), R(oom), D(ay), S(tart), E(nd).
   - courses: final list that will get returned. */
constrain_courses([], [], [], _).

/* Constrain the values for the courses. */
constrain_courses([[Prof, Class]|Rest],[Day, Start, Room|Variables], [course(Prof, Class, Room, Day, Start)|CourseList], Constraints):-

  /* There are 5 working days a week (monday, tuesday, etc.)*/
  Day in 1..5,
  Start in 9..14,
  Room in 100..102,
  %teaches(Prof, Class, Constraints),
  %Start #= 11,
  inRoom(Constraints, Class, Room),

  /* Search how much students given class has. */
  has(Class, NoStudents, Constraints),
  seats(Room, NoSeats),
  NoStudents #=< NoSeats,

  /* Next iteration. */
  constrain_courses(Rest, Variables, CourseList, Constraints).


compare_all(_, [], _).


compare_all(course(Prof1, Class1, Room1, Day1, Start1),
             [course(Prof2, Class2, Room2, Day2, Start2)|Courses], Constraints):-

                /* Compare courses */
                (Prof1 #= Prof2 #/\ Day1 #= Day2) #==> Start1 #>= Start2 + 2,
                (Day1 #= Day2 #/\ Room1 #= Room2) #==> #\(Start1 #= Start2),
                (Room1 #= Room2 #/\ Start1 #= Start2) #==> #\(Day1 #= Day2),
                (Class1 #= Class2) #==> #\(Prof1 #= Prof2),

                sameDay(course(Prof1, Class1, Room1, Day1, Start1), course(Prof2, Class2, Room2, Day2, Start2), Constraints), !,
                before(course(Prof1, Class1, Room1, Day1, Start1), course(Prof2, Class2, Room2, Day2, Start2), Constraints), !,
                after(course(Prof1, Class1, Room1, Day1, Start1), course(Prof2, Class2, Room2, Day2, Start2), Constraints), !,

                compare_all(course(Prof1, Class1, Room1, Day1, Start1), Courses, Constraints).

  /* Link the courses together. */
  link_courses([], _).
  link_courses([_],_).

  /* Check the constraints for the relations between classes. */
  link_courses([course(Prof1, Class1, Room1, Day1, Start1),
                course(Prof2, Class2, Room2, Day2, Start2)|Courses], Constraints):-

      /* Compare the first course with all other courses. */
      compare_all(course(Prof1, Class1, Room1, Day1, Start1), [course(Prof2, Class2, Room2, Day2, Start2)|Courses], Constraints),

      link_courses([course(Prof2, Class2, Room2, Day2, Start2)|Courses], Constraints).


/* Final timetable */
timetable(Data, Timetable):-

  sentences(Constraints, Data, []),

  /* Process the constraints (replace he and she by their real value) */
  processConstraints(Constraints, [], P),

  %write("constraints: "), write(Constraints), nl,
  make_class_prof_list(P, [], Pairs),
  %write("pairs: "), write(Pairs), nl,

  length(Pairs, PairsLen),
  length(Timetable, PairsLen), !,

  /* The idea comes from the resource allocation from Chapter 15.
   * In it, the predicate 'constrain_boxes' takes in a list of boxnumbers
   * to uniquely identify the boxes.
   * To uniquely identify a course: we need its professor and class (pairs). */
  constrain_courses(Pairs, Variables, Timetable, P),
  link_courses(Timetable, Constraints),!,
  labeling([ffc], Variables).

/* 3. PRINTING THE RESULTS
 * The following predicates implement 'print_timetable' that displays
 * a timetable over 5 days and 5 hours. */

/* Header for the timetable */
print_header:- nl, write("  -------------------------------------------------------------------------------------------------------------------------------------------------------"), nl,
               print("|       |           DAY 1           |           DAY 2            |           DAY 3            |           DAY 4            |           DAY 5            |"), nl.



/* Find a course at a certain day, hour and room. */
findCourse([], _, _, _, []).
findCourse([course(Prof, Class, Room, Day, Hour)|_], Day, Hour, Room, course(Prof, Class, Room, Day, Hour)).
findCourse([course(_, _, _, _, _)|Rest], OtherDay, OtherHour, OtherRoom, Z):-
findCourse(Rest, OtherDay, OtherHour, OtherRoom, Z).

  /* Following predicate will print out a course */
  printCourse([]):- write("                          "). % If the course is empty, print nothing.
  printCourse(course(Prof, Class, Room, _, _)):-
    write("Cl: "), real_class(Class, RealC), write(RealC), write(", "), write("Pr: "),
    real_prof(Prof, RealProf), write(RealProf), write(", "), write("Ro: "), write(Room).

  /* Correct the placement of an hour in the timetable: if it's smaller than 10, it needs an additional white space. */
  correct_hour(H):- H < 10, write(" "), write(H).
  correct_hour(H):- write(H).

  print_table_hour(_, 15).
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

/*
* 4. TESTS
* What follows is a series of tests that show the implementation's effectiveness.
*
* There are 6 tests:
* -parser_test: this predicate takes in data and shows the DCG parser works to start with.
*
* -timetable_50_test: forms a timetable of 50 courses for 10 given professors teaching 5 classes each.
*  dispersed under 3 rooms: those were part of the general rules for the assignment.
*
* -timetable_same_day_test: since -for 50 courses- it is impossible to schedule for instance 2 classes of 10
*  professors, this test shows that it's still managable to schedule up to 10 courses for 2 classes together in the same day.
*
* -timetable_fail_same_day_test: this test shows the solver can fail if there are too much courses to cram in the same day.
*
* -print_solution: print out a timetable with the implemented print predicate for the 'timetable_50' dataset.
*
* -all_tests: try all tests in succession.
*/
timetable_50([
           prof, jerry, teaches, class, a1, c1, c2, c3, c4, fullstop,
           prof, smith, teaches, class, a1, c1, c2, c3, c4, fullstop,
           prof, jones, teaches, class, a1, c1, c2, c3, c4, fullstop,
           prof, lopez, teaches, class, a1, c1, c2, c3, c4, fullstop,
           prof, jager, teaches, class, a1, c1, c2, c3, c4, fullstop,

           prof, sevic, teaches, class, a1, c1, c2, c3, c4, fullstop,
           prof, stark, teaches, class, a1, c1, c2, c3, c4, fullstop,
           prof, chome, teaches, class, a1, c1, c2, c3, c4, fullstop,
           prof, acker, teaches, class, a1, c1, c2, c3, c4, fullstop,
           prof, frost, teaches, class, a1, c1, c2, c3, c4, fullstop,

           /* Specify the capacity of the rooms. */
           room, 100, seats, 35, students, fullstop,
           room, 101, seats, 60, students, fullstop,
           room, 102, seats, 100, students, fullstop,

           /* Number of students per class. */
           class, c1, has, 30, students, fullstop,
           class, c2, has, 35, students, fullstop,
           class, c3, has, 100, students, fullstop,
           class, c4, has, 40, students, fullstop,
           class, a1, has, 50, students, fullstop

           ]).


timetable_same_day([
          prof, jerry, teaches, class, a1, c1, c2, c3, c4, fullstop,
          prof, smith, teaches, class, a1, c1, c2, c3, c4, fullstop,
          prof, jones, teaches, class, a1, c1, c2, c3, c4, fullstop,
          prof, lopez, teaches, class, a1, c1, c2, c3, c4, fullstop,

          /* Specify the capacity of the rooms. */
          room, 100, seats, 35, students, fullstop,
          room, 101, seats, 60, students, fullstop,
          room, 102, seats, 100, students, fullstop,

          /* Number of students per class. */
          class, c1, has, 30, students, fullstop,
          class, c2, has, 35, students, fullstop,
          class, c3, has, 100, students, fullstop,
          class, c4, has, 40, students, fullstop,
          class, a1, has, 50, students, fullstop,

          /* Specify room locations for specific classes. */
          class, c1, is, in, room, 102, fullstop,

          /* Specifications for all classes. */
          classes, c1, and, c4, are, on, the, same, day, fullstop
                      ]).

timetable_same_day_fail([
          prof, jerry, teaches, class, a1, c1, c2, c3, c4, fullstop,
          prof, smith, teaches, class, a1, c1, c2, c3, c4, fullstop,
          prof, jones, teaches, class, a1, c1, c2, c3, c4, fullstop,
          prof, lopez, teaches, class, a1, c1, c2, c3, c4, fullstop,

          prof, sevic, teaches, class, a1, c1, c2, c3, c4, fullstop,
          prof, stark, teaches, class, a1, c1, c2, c3, c4, fullstop,
          prof, chome, teaches, class, a1, c1, c2, c3, c4, fullstop,
          prof, acker, teaches, class, a1, c1, c2, c3, c4, fullstop,
          prof, frost, teaches, class, a1, c1, c2, c3, c4, fullstop,

          /* Specify the capacity of the rooms. */
          room, 100, seats, 35, students, fullstop,
          room, 101, seats, 60, students, fullstop,
          room, 102, seats, 100, students, fullstop,

          /* Number of students per class. */
          class, c1, has, 30, students, fullstop,
          class, c2, has, 35, students, fullstop,
          class, c3, has, 100, students, fullstop,
          class, c4, has, 40, students, fullstop,
          class, a1, has, 50, students, fullstop,

          /* Specify room locations for specific classes. */
          class, c1, is, in, room, 102, fullstop,

          /* Specifications for all classes. */
          classes, c1, and, c4, are, on, the, same, day, fullstop ]).

parser([
          prof, jerry, teaches, class, a1, c1, c2, c3, c4, fullstop,
          prof, smith, teaches, class, a1, c1, c2, c3, c4, fullstop,
          prof, jones, teaches, class, a1, c1, c2, c3, c4, fullstop,

          /* Specify the capacity of the rooms. */
          room, 100, seats, 35, students, fullstop,
          room, 101, seats, 60, students, fullstop,
          room, 102, seats, 100, students, fullstop,

          /* Number of students per class. */
          class, c1, has, 30, students, fullstop,
          class, c2, has, 35, students, fullstop,
          class, c3, has, 100, students, fullstop,

          /* Time specifications for classes. */
          class, c4, is, before, class, c1, fullstop,
          class, c4, is, after, class, c3, fullstop,
          classes, c1, and, c4, are, on, the, same, day, fullstop,

          /* Specify room locations for specific classes. */
          class, c1, is, in, room, 102, fullstop ]).


/* Here are the final tests */
parser_test:- parser(Data), sentences(Constraints, Data, []),
              nl, write("parser_test succeeded."), nl.
parser_test:- nl, write("parser_test failed."), nl.

timetable_50_test:- timetable_50(Data), timetable(Data, _),
                  nl, write("timetable_50_test succesful."), nl.
timetable_50_test:- write("timetable_50_test unsuccesful."), nl.

timetable_same_day_test:- timetable_50(Data), timetable(Data, _),
                          nl, write("timetable_50_test succesful."), nl.
timetable_same_day_test:- write("timetable_50_test unsuccesful.").

timetable_fail_same_day_test:- timetable_same_day_fail(Data), timetable(Data, Timetable),
                               nl, write("timetable_fail_same_day_test succesful: that wasn't supposed to happen."), nl.
timetable_fail_same_day_test:- nl, write("timetable_fail_same_day_test failed as expected."), nl.

print_solution:- timetable_50(Data), timetable(Data, Timetable),print_table(Timetable),
                  nl, write("print_solution test succesful."), nl.
print_solution:- write("print_solution test unsuccesful.").

all_tests:- parser_test, timetable_50_test, timetable_same_day_test, timetable_fail_same_day_test, print_solution.
