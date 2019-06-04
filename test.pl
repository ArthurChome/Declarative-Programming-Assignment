
% TESTS%

/* Import following libraries for the code to work: */
:- use_module(library(lists)).
:- use_module(library(clpfd)).

%:- block box( -, - ).
box( 1, 2 ).
box( 2, 2 ).
box( 3, 6 ).
box( 4, 2 ).
box( 5, 4 ).
box( 6, 2 ).
box( 7, 6 ).
box( 8, 5 ).
box( 9, 3 ).

constrain_boxes( [], [], [] ).
constrain_boxes( [Box|Boxes], [B,T|Variables], [box( Box,B,T,H,Stack )|BoxList] ) :-
  T in 0..20,
  B in 0..20,
  H in 2..6,
  T #= B + H,
%T #= 5,
Stack in 1..4,

constrain_boxes( Boxes, Variables, BoxList ).

link_boxes( [] ).
link_boxes( [_] ).

link_boxes( [box(_,_, T1, H1, Stack1 ),
box( Box2, B2, T2, H2, Stack2 )|Boxes] ) :-
  %Stack2 #>= Stack1,
  %Stack2 #=< Stack1 + 1,
  %( Stack1 #= Stack2 ) #=> ( H1 #>= H2 ),
  %( Stack1 #= Stack2 ) #<=> ( T1 #= B2 ),
  %( Stack1 #= Stack2 - 1 ) #<=> ( B2 #= 0 ),
link_boxes( [box( Box2, B2, T2, H2, Stack2 )|
Boxes] ).

store_boxes( BoxList ) :-

length( BoxList, 9 ),
length( BoxNumbers, 9 ),
all_different( BoxNumbers ),

constrain_boxes( BoxNumbers, Variables, BoxList ),
link_boxes( BoxList ),
labeling( [ffc], Variables ).

test :- store_boxes(Box), write("boxes: "), write(Box).
