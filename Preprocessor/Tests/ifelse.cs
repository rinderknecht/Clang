#define TRUE

A
#if TRUE
B
#else
C
  #if TRUE
D
  #else
E
  #endif
F
#endif

G

#if TRUE
H
#else
I
  #if FALSE
J
  #else
K
  #endif
L
#endif

M

#if FALSE
N
#else
O
  #if TRUE
P
  #else
Q
  #endif
R
#endif

S

#if FALSE
T
#else
U
  #if FALSE
V
  #else
W
  #endif
X
#endif

Y
