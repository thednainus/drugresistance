#drug resistance in SARS-CoV-2
#based on this model
#chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.fields.utoronto.ca/programs/scientific/10-11/drugresistance/emergence/fred2.pdf


##################################################################################
model2=function(t, x, model_parameters){

  # The vector x is the same length as the number of compartments.  In your main
  # program you identify which compartment corresponds to which element of x.
  # You need to make sure that these are in the same order.

  S = x[1]
  Iw = x[2]
  Tw = x[3]
  Ir = x[4]
  Tr = x[5]
  R = x[6]

  # model_parameters is a list object, filled in the main program, and passed
  # Keep this next line the same when you are writing your own function to
  # solve a system of ODE's

  with(as.list(model_parameters),{


    # calculate the population size, which for our model is
    # npop = S+Iw+Tw+Ir+Tr+R
    # we will need this to calculate our model derivatives below

    npop = S+Iw+Tw+Ir+Tr+R

    # Now give the expressions for the derivatives of S, Iw, Ir, Tw, Tr, R with
    # time.
    # these come from the model equations.  The following equations are for
    # an SITR model.  When you write your own function, replace these with
    # your model equations
    #dIw = +S/npop*( beta_w*(Iw + delta*Tw) ) -alpha*Iw -gamma_w*Iw
    #dS = -S/npop*( beta_w*(Iw + delta*Tw) +beta_r*(Ir + Tr) )
    #dTw = +alpha*Iw - phi*Tw -eta*Tw
    #dIr = +S/npop*( beta_r*(Ir + Tr) ) -alpha*Ir -gamma_r*Ir
    #dTr = +alpha*Ir + phi*Tw - gamma_r*Tr
    #dR = +gamma_w*Iw + gamma_r*Ir + eta*Tw +gamma_r*Tr

    dS = -S*( beta_w*(Iw + delta*Tw) +beta_r*(Ir + Tr) )
    dIw = +S*( beta_w*(Iw + delta*Tw) ) -alpha*Iw -gamma_w*Iw
    dTw = +alpha*Iw - phi*Tw -eta*Tw
    dIr = +S*( beta_r*(Ir + Tr) ) -alpha*Ir -gamma_r*Ir
    dTr = +alpha*Ir + phi*Tw - gamma_r*Tr
    dR = +gamma_w*Iw + gamma_r*Ir + eta*Tw +gamma_r*Tr

    ############################################################################
    # vout is an output vector that contains the values of the derivates
    # the compartments in the vector on the RHS need to be in the same order as the
    # compartments used to fill the x vector!
    ############################################################################
    vout = c(dS,dIw,dTw,dIr,dTr,dR)
    list(vout)
  })
}
