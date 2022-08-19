#drug resistance in SARS-CoV-2


#this is very simple model
#function was inspired in https://sherrytowers.com/2012/12/11/simple-epidemic-modelling-with-an-sir-model/
#base script was downloaded from the above website on 8 August 2022

##################################################################################
model1=function(t, x, model_parameters){

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

    dS = -beta_w*(Iw + (rho_w*Tw))*S/npop -beta_r*(Ir + (rho_r*Tr))*S/npop
    dIw = +beta_w*(Iw + (rho_w*Tw))*S/npop + pi*Ir - alpha*Iw
    dTw = +alpha*Iw - phi*Tw - gamma_w*Tw
    dIr = +beta_r*(Ir + (rho_r*Tr))*S/npop - alpha*Ir - pi*Ir
    dTr = +alpha*Ir + phi*Tw - gamma_r*Tr
    dR = +gamma_w*Tw + gamma_r*Tr


    # vout is an output vector that contains the values of the derivates
    # the compartments in the vector on the RHS need to be in the same order as the
    # compartments used to fill the x vector!

    vout = c(dS,dIw,dTw,dIr,dTr,dR)
    list(vout)
  })
}
