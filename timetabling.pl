%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                               %
%      DECLARATIVE PROGRAMMING:TIMETABLING      %
%                By Arthur Chomé                %
%                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Import following libraries for the code to work: */
:- use_module(library(lists)).
:- use_module(library(clpfd)).

/* For the arguments, we go from the assumption that 'verb_phrase' gives back */
sentences([Constr1|Constr2]) --> sentence(Constr1), sentences(Constr2).
sentences([]) --> sentence(_), sentences(_).

/* It's also possible that you have no sentences left. */
sentences([]) --> [].
sentence([A,NP,B]) --> noun_phrase(NP), verb_phrase([A,B]), fullstop, {A\= is, A\= are}.

/* If the verb is empty, the objects contain the time (before or after) for the subject. */
sentence([B,NP,C]) --> noun_phrase(NP), verb_phrase([_,[B|C]]), fullstop.


/* Appending an empty list with a list equals the list that has at least some content. */
noun_phrase([]) --> [].
noun_phrase(C1) --> noun(C1), noun(_).
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
noun(A) --> [the], noun(A).
noun(A) --> [same], noun(A).

/* Possible classes for the school: you only have 5 classes.
 * We have to give them numerical values: c1 = 1, c2 = 2, c3 = 3, c4 = 4, a1 = 5 */
noun([1|A]) --> [c1], noun(A).
noun([2|A]) --> [c2], noun(A).
noun([3|A]) --> [c3], noun(A).
noun([4|A]) --> [c4], noun(A).
noun([5|A]) --> [a1], noun(A).

/* Numbers that can be used as subjects or objects
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

/* Name of the professors
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
 * as to know which person (e.g. professor) the sentence is talking about. */
 noun(he) --> [he], noun(_).
 noun(she) --> [she], noun(_).
 noun(sameTeacher) --> [the, same, teacher], noun(_).

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

%newSentence(_,_,_):- write("new sentence"), nl.

/* Every class has an amount of students.
 * Remember: c1 = 1, c2 = 2, c3 = 3, c4 = 4 and a1 = 5
 * Can be handy when we're solving constraints. */
%has(1, 30).
%has(2, 35).
%has(3, 100).
%has(4, 40).
%has(5, 50).

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

/* BASIC LIST PROCESSING AND EQUALITY ALGORITHMS */

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
 isEqual(A, [A|_]):- write("Is equal: "), write(A), nl.
 isEqual(A, [B|C]):- write("is not equal: "), write(A), write(" and "), write(B), nl, isEqual(A, C).


 /* Following predicates test if the constraints are even possible with given timetable. */
 /* For a given class, the predicate counts how much different teachers it has. */
 %count_teachers_class([], Class, Count, Count):- write("teachers class count: "), write(Count).
 %count_teachers_class([[teaches, Prof, Classes]|Rest], Class, Count, Z):-
%   find(Classes, Class),%%
%   write("aaa"), nl,
%   NewCount is Count + 1,
%   write("AAAA"), nl,
%   count_teachers_class(Rest, Class, NewCount, Z).

 %count_teachers_class([Constraint|Rest], Count, Z):- write("did not find such a a thing"), nl, count_teachers_class(Rest, Count, Z).
 find_a_class(Classes, [], Cnt, Cnt).
 find_a_class(Classes, [A|OtherClasses], Cnt, Z):- find(Classes, A), NewCnt is Cnt + 1, find_a_class(Classes, OtherClasses, NewCnt, Z).
 find_a_class(Classes, [A|OtherClasses], Cnt, Z):- find_a_class(Classes, OtherClasses, Cnt, Z).

 /* If you found a day*/
 count_courses_same_day([], Classes, Count, Count):- write("Final count_courses_same_day count: "), write(Count), nl.
 count_courses_same_day([[teaches, Prof, ProfClasses]|OtherConstraints], Classes, Count, Z):-
   find_a_class(ProfClasses, Classes, 0, Total),
   NewCount is Count + Total,
   count_courses_same_day(OtherConstraints, Classes, NewCount, Z).


count_courses_same_day([Constraint|OtherConstraints], Classes, NewCount, Z):- count_courses_same_day(OtherConstraints, Classes, NewCount, Z).

 /* Upper bound is 10 courses a day for 3 rooms and days of 5 hours. */
 same_day_possible([], Constraints):- write("same day possible"), nl.

 same_day_possible([[sameDay, Classes, Nothing]|Rest], Constraints):-
   count_courses_same_day(Constraints, Classes, 0, Count),
   Count <= 8,
   same_day_possible(Rest, Constraints).

   same_day_possible([[sameDay, Classes, Nothing]|Rest], Constraints):-
     count_courses_same_day(Constraints, Classes, 0, Count),
     Count > 8, halt.

 same_day_possible([[A|Rest], Constraints):- write("continue"), nl, same_day_possible(Rest, Constraints).


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
searchSameDay([A|B], Verb, Pairs, Z):- first(A, FoundVerb), second(A, Second), third(A, Third),
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
sameDayClasses(Class1, Class2, [], []).
sameDayClasses(Class1, Class2, [[A, _]|_], A):- find(A, Class1), find(A, Class2).
sameDayClasses(Class1, Class2, [[_, _]|OtherPairs], Z):- sameDayClasses(Class1, Class2, OtherPairs, Z).


/* Process the list of class pairs given a class */
findRoom([], _, []).
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
inRoom(Constraints, Class, _, AnswerRoom):- searchConstraints(Constraints, inRoom, [], Pairs),
                                                      %write("pairs: "), write(Pairs), nl,
                                                        findRoom(Pairs, Class, AnswerRoom),
                                                      %  write("answer room: "), write(AnswerRoom), nl,
                                                        \+ isEqual(AnswerRoom, []).
inRoom(_, _, OriginalRoom, OriginalRoom).


before(course(Class1, _, _, _, Start1), course(Class2, _, _, _, Start2), Constraints):-

  searchConstraints(Constraints, before, [], Pairs),
  searchPairs(Class1, Class2, Pairs),

   Start1 #< Start2.

before(course(_, _, _, _, _), course(_, _, _, _, _), _).

/* Constraint that some classes need to take place after some other classes. */
after(course(Class1, _, _, _, Start1), course(Class2, _, _, _, Start2), Constraints):-
  searchConstraints(Constraints, after, [], Pairs),

  searchPairs(Class1, Class2, Pairs),
  Start1 #> Start2.

after(course(_, _, _, _, _), course(_, _, _, _, _), _).

teaches(Prof, Class, Constraints):-
  searchConstraints(Constraints, teaches, [], Pairs),

  searchPairs(Prof, Class, Pairs).


%:- block course(-,-,-,-,-).

/* Put the constraints on the courses.
   Arguments:
   - coursenumbers: 10 teachers x 5 classes = 50 numbers
   - variables for each course: C(lass), R(oom), D(ay), S(tart), E(nd).
   - courses: final list that will get returned. */
constrain_courses([], [], [], Constraints).


/* Constrain the values for the courses. */
constrain_courses([[Prof, Class]|Rest],[Day, Start, Room|Variables], [course(Prof, Class, Room, Day, Start)|CourseList], Constraints):-

  /* There are 5 working days a week (monday, tuesday, etc.)*/
  Day in 1..5,
  Start in 9..14,
  Room in 100..102,
  %teaches(Prof, Class, Constraints),
  %Start #= 11,

  /* Search how much students given class has. */
  has(Class, NoStudents, Constraints),
  seats(Room, NoSeats),
  %inRoom(Constraints, Class,Room, AnswerRoom),
  NoStudents #=< NoSeats,

  /* Next iteration. */
  constrain_courses(Rest, Variables, CourseList, Constraints).


compare_all(_, [], Constraints).


compare_all(course(Prof1, Class1, Room1, Day1, Start1),
             [course(Prof2, Class2, Room2, Day2, Start2)|Courses], Constraints):-

                /* Time constraints */
                sameDay(course(Prof1, Class1, Room1, Day1, Start1), course(Prof2, Class2, Room2, Day2, Start2), Constraints),
                %before(course(Prof1, Class1, Room1, Day1, Start1), course(Prof2, Class2, Room2, Day2, Start2), Constraints),
                %after(course(Prof1, Class1, Room1, Day1, Start1), course(Prof2, Class2, Room2, Day2, Start2), Constraints),

                /* Compare courses */
                (Prof1 #= Prof2 #/\ Day1 #= Day2) #==> (Start1 #>= Start2 + 2),
                (Day1 #= Day2 #/\ Room1 #= Room2) #==> #\(Start1 #= Start2),
                (Room1 #= Room2 #/\ Start1 #= Start2) #==> #\(Day1 #= Day2),
                (Class1 #= Class2) #==> #\(Prof1 #= Prof2),
                %(Prof1 #= Prof2 #/\ Day1 #= Day2) #==> (Start1 #>= Start2 + 2),
                %(Day1 #= Day2 #/\ Room1 #= Room2) #==> #\(Start1 #= Start2),
                %(Room1 #= Room2 #/\ Start1 #= Start2) #==> #\(Day1 #= Day2),
                %(Day1 #= Day2 #/\ Start1 #= Start2) #==> #\(Room1 #= Room2),
                %(Class1 #= Class2) #==> #\(Prof1 #= Prof2),
                %Start1 \#= Start2,
                %(Start1 #= Start2) #==> #\(Day1 #= Day2),
                %(Day1 #= Day2) #==> #\(Start1 #= Start2),
                write("compare: "), !,
                %(Class1 #= Class2) #==> #\(Prof1 #= Prof2),

                compare_all(course(Prof1, Class1, Room1, Day1, Start1), Courses, Constraints).


  /* For same day to work, there should be not too much classes for a day. */
  sameDay(course(Prof1, Class1, Room1, Day1, Start1), course(Prof2, Class2, Room2, Day2, Start2), Constraints):-

    %Start1 #=< 14,
    %Start2 #=< 14,
    searchSameDay(Constraints, sameDay, [], Pairs),
    searchDay(Class1, Class2, Pairs), !,
    %write("constraints: "), write(Constraints), nl,
    %write("pairs of same day: "), write(Pairs), nl,
    %sameDayClasses(Class1, Class2, Pairs, Classes),
    %count_courses_same_day(Constraints, Classes, 0, Count),
    %write("number of courses in a same day: "), write(Count), nl,
    %Count <= 8,

    Day1 #= Day2.

  sameDay(course(Prof1, Class1, Room1, Day1, Start1), course(Prof2, Class2, Room2, Day2, Start2), Constraints).

  /* Link the courses together. */
  link_courses([], Constraints).
  link_courses([_], Constraints).

  /* Check the constraints for the relations between classes. */
  link_courses([course(Prof1, Class1, Room1, Day1, Start1),
                course(Prof2, Class2, Room2, Day2, Start2)|Courses], Constraints):-

      /* Compare the first course with all other courses. */
      compare_all(course(Prof1, Class1, Room1, Day1, Start1), [course(Prof2, Class2, Room2, Day2, Start2)|Courses], Constraints),

      link_courses([course(Prof2, Class2, Room2, Day2, Start2)|Courses], Constraints).


/* Final timetable */
timetable(Data, Timetable):-

  sentences(Constraints, Data, []),
  write("constraints: "), write(Constraints), nl,
  %processConstraints(Constraints, [], P),
  make_class_prof_list(Constraints, [], Pairs),
  write("pairs: "), write(Pairs), nl,

  %sentences(Constraints, Data, []),
  /* Process the constraints (replace he and she by their real value) */
  %processConstraints(Constraints, [], P),

  /* Check if given constraints are possible. */
  %same_day_possible(Constraints, Constraints),


  /* Debugger */
  %nl, write("Processed constraints: "), write(P), nl,

  /* It's actually a list of prof-class paris*/
  %make_class_prof_list(Constraints, [], Pairs),

  length(Pairs, PairsLen),
  length(Timetable, PairsLen), !,
  %PairsLen = TimetableLen,



  /* The idea comes from the resource allocation from Chapter 15.
   * In it, the predicate 'constrain_boxes' takes in a list of boxnumbers
   * to uniquely identify the boxes.
   * To uniquely identify a course: we need its professor and class (pairs). */
  constrain_courses(Pairs, Variables, Timetable, Constraints),
  link_courses(Timetable, Constraints), !,
  labeling([ffc], Variables).

%timetable(Data, Timetable):- write("could not find. ").

/* TIME TABLE PRINTER */

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


/* TESTS */

test([
           prof, jerry, teaches, class, a1, c1, c2, c3, c4, fullstop,
           prof, smith, teaches, class, a1, c1, c2, c3, c4, fullstop,
           prof, jones, teaches, class, a1, c1, c2, c3, c4, fullstop,
           prof, lopez, teaches, class, a1, c1, c2, c3, c4, fullstop,
           %prof, jager, teaches, class, a1, c1, c2, c3, c4, fullstop,

           %prof, sevic, teaches, class, a1, c1, c2, c3, c4, fullstop,
           %prof, stark, teaches, class, a1, c1, c2, c3, c4, fullstop,
           %prof, chome, teaches, class, a1, c1, c2, c3, c4, fullstop,
           %prof, acker, teaches, class, a1, c1, c2, c3, c4, fullstop,
           %prof, frost, teaches, class, a1, c1, c2, c3, c4, fullstop,

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


           /* Specifications for all classes. */
           %class, c4, is, before, class, c1, fullstop
           %class, c4, is, after, class, c3, fullstop
           classes, c1, and, c4, are, on, the, same, day, fullstop
           ]).

solution :- test(Data), timetable(Data, Timetable), write("timetable: "), write(Timetable), print_table(Timetable)
.
test:- test(Data),sentences(Constraints, Data, []), write("constraints: "), write(Constraints).
