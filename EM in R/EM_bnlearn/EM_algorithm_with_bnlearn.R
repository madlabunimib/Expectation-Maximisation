library(bnlearn)

# https://www.bnlearn.com/documentation/man/impute.html
# https://www.bnlearn.com/documentation/man/structural.em.html

# incomplete data
lv = c("0", "1")
x <- data.frame("A" = factor(c(0, 0, 0, NA, NA, NA, 1, 1, 1, 1), levels = lv),
                "B" = factor(c(0, 1, 1, 1, 0, 0, 0, 0, 1, NA), levels = lv))

# FIRST CASE
# a simple BN with two nodes A and B linked by a single arc
em.dag = empty.graph(nodes = c("A", "B"))
em.dag = set.arc(em.dag, from = "A", to = "B")
path(em.dag, from = "A", to = "B")

em.dag


# parameters of the local distribution of A
A.prob = array(c(0.5, 0.5), dim = 2, dimnames = list(A = lv))
# parameters of the local distribution of B
B.prob = array(c(0.333, 0.667, 0.667, 0.333), dim = c(2, 2),
               dimnames = list(B = lv, A = lv))
cpt = list(A = A.prob, B = B.prob)
cpt

bn = custom.fit(em.dag, cpt)
bn

# expectation step
imputed = impute(bn, x, method = "bayes-lw")
imputed

# maximisation step (forcing A to be connected to B)
em.dag = tabu(imputed, whitelist = data.frame(from = "A", to = "B"))
bn = bn.fit(em.dag, imputed, method = "bayes")
bn


# SECOND CASE
# initialise an empty BN
imputed = x
bn = bn.fit(empty.graph(names(x)), imputed)
bn

bn$A = array(c(0.5, 0.5), dim = 2, dimnames = list(lv))
# I can't initialize the parameters of the local distribution of B
# because I have not an arc from A to B

for (i in 1:4) {
  # expectation step
  imputed = impute(bn, x, method = "bayes-lw")
  imputed
  
  # maximisation step (forcing A to be connected to B,
  # and not to the other nodes because A create a self-loop)
  dag = tabu(imputed, whitelist = data.frame(from = "A", to = "B"))
  dag
  #graphviz.plot(dag)
  
  bn = bn.fit(dag, imputed, method = "bayes")
  bn
  # same results of first case
}

imputed
dag

bn


# THIRD CASE
# structural.em function
r = structural.em(x, fit = "bayes", fit.args = list(), maximize = "tabu",
                  maximize.args = list(whitelist = data.frame(from = "A", to = "B")), 
                  "parents", impute.args = list(), return.all = TRUE,
                  start = NULL, max.iter = 4, debug = FALSE)

r[2]
r[1]
r[3]

