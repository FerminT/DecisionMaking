#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jul 12 14:13:24 2019

[translated from fit_meta_d_MLE.m by Maniscalco & Lau (2012) by alan lee]
[requires numpy-1.13.3, scipy-1.1.0, or later versions]
[comments below are copied and pasted from fit_rs_meta_d_MLE.m]

% fit = fit_rs_meta_d_MLE(nR_S1, nR_S2, s, fncdf, fninv)
%
% Given data from an experiment where an observer discriminates between two
% stimulus alternatives on every trial and provides confidence ratings,
% provides a RESPONSE-SPECIFIC type 2 SDT analysis of the data. i.e.
% meta-d' is computed separately for each response type. 
%
% N.B. it is still important to provide input including data from both 
% response types, even if you are only interested in the analysis for one 
% of the response types.
% 
% 
%
% INPUTS
%
% * nR_S1, nR_S2
% these are vectors containing the total number of responses in
% each response category, conditional on presentation of S1 and S2.
%
% e.g. if nR_S1 = [100 50 20 10 5 1], then when stimulus S1 was
% presented, the subject had the following response counts:
% responded S1, rating=3 : 100 times
% responded S1, rating=2 : 50 times
% responded S1, rating=1 : 20 times
% responded S2, rating=1 : 10 times
% responded S2, rating=2 : 5 times
% responded S2, rating=3 : 1 time
%
% The ordering of response / rating counts for S2 should be the same as it
% is for S1. e.g. if nR_S2 = [3 7 8 12 27 89], then when stimulus S2 was
% presented, the subject had the following response counts:
% responded S1, rating=3 : 3 times
% responded S1, rating=2 : 7 times
% responded S1, rating=1 : 8 times
% responded S2, rating=1 : 12 times
% responded S2, rating=2 : 27 times
% responded S2, rating=3 : 89 times
%
% N.B. if nR_S1 or nR_S2 contain zeros, this may interfere with estimation of
% meta-d'.
%
% Some options for dealing with response cell counts containing zeros are:
% 
% (1) Add a small adjustment factor, e.g. adj_f = 1/(length(nR_S1), to each 
% input vector:
% 
% adj_f = 1/length(nR_S1);
% nR_S1_adj = nR_S1 + adj_f;
% nR_S2_adj = nR_S2 + adj_f;
% 
% This is a generalization of the correction for similar estimation issues of
% type 1 d' as recommended in
% 
% Hautus, M. J. (1995). Corrections for extreme proportions and their biasing 
%     effects on estimated values of d'. Behavior Research Methods, Instruments, 
%     & Computers, 27, 46-51.
%     
% When using this correction method, it is recommended to add the adjustment 
% factor to ALL data for all subjects, even for those subjects whose data is 
% not in need of such correction, in order to avoid biases in the analysis 
% (cf Snodgrass & Corwin, 1988).
% 
% (2) Collapse across rating categories.
% 
% e.g. if your data set has 4 possible confidence ratings such that length(nR_S1)==8,
% defining new input vectors
% 
% nR_S1_new = [sum(nR_S1(1:2)), sum(nR_S1(3:4)), sum(nR_S1(5:6)), sum(nR_S1(7:8))];
% nR_S2_new = [sum(nR_S2(1:2)), sum(nR_S2(3:4)), sum(nR_S2(5:6)), sum(nR_S2(7:8))];
% 
% might be sufficient to eliminate zeros from the input without using an adjustment.
%
% * s
% this is the ratio of standard deviations for type 1 distributions, i.e.
%
% s = sd(S1) / sd(S2)
%
% if not specified, s is set to a default value of 1.
% For most purposes, we recommend setting s = 1. 
% See http://www.columbia.edu/~bsm2105/type2sdt for further discussion.
%
% * fncdf
% a function handle for the CDF of the type 1 distribution.
% if not specified, fncdf defaults to @normcdf (i.e. CDF for normal
% distribution)
%
% * fninv
% a function handle for the inverse CDF of the type 1 distribution.
% if not specified, fninv defaults to @norminv
%
% OUTPUT
%
% Output is packaged in the struct "fit." 
% In the following, let S1 and S2 represent the distributions of evidence 
% generated by stimulus classes S1 and S2.
% Then the fields of "fit" are as follows:
% 
% fit.da        = mean(S2) - mean(S1), in room-mean-square(sd(S1),sd(S2)) units
% fit.t1ca      = type 1 criterion for overall data, RMS units
% fit.s         = sd(S1) / sd(S2)
%
% fit.meta_da_rS1   = meta-d' for S1 responses, RMS units
% fit.t1ca_rS1      = type 1 criteron for meta-d' fit for S1 responses, RMS units
% fit.t2ca_rS1      = type 2 criteria for meta-d' fit for S1 responses, RMS units
% fit.M_ratio_rS1   = meta_da_rS1 / da
% fit.M_diff_rS1    = meta_da_rS1 - da
%
% fit.logL_rS1      = log likelihood of meta-d' fit for S1 responses
%
% fit.obs_HR2_rS1   = actual type 2 hit rates for S1 responses
% fit.est_HR2_rS1   = estimated type 2 hit rates for S1 responses
% fit.obs_FAR2_rS1  = actual type 2 false alarm rates for S1 responses
% fit.est_FAR2_rS1  = estimated type 2 false alarm rates for S1 responses
%
%
% fit.meta_da_rS2   = meta-d' for S2 responses, RMS units
% fit.t1ca_rS2      = type 1 criteron for meta-d' fit for S2 responses, RMS units
% fit.t2ca_rS2      = type 2 criteria for meta-d' fit for S2 responses, RMS units
% fit.M_ratio_rS2   = meta_da_rS2 / da
% fit.M_diff_rS2    = meta_da_rS2 - da
%
% fit.logL_rS2      = log likelihood of meta-d' fit for S2 responses
%
% fit.obs_HR2_rS2   = actual type 2 hit rates for S2 responses
% fit.est_HR2_rS2   = estimated type 2 hit rates for S2 responses
% fit.obs_FAR2_rS2  = actual type 2 false alarm rates for S2 responses
% fit.est_FAR2_rS2  = estimated type 2 false alarm rates for S2 responses
%
%
% fit.S1units   = contains same parameters in sd(S1) units.
%                 these may be of use since the data-fitting is conducted  
%                 using parameters specified in sd(S1) units.
%
% If there are N ratings, then there will be N-1 type 2 hit rates and false
% alarm rates. 

% 2019/07/12 - translated to python3 by alan lee
                [requires numpy-1.13.3, scipy-1.1.0, or later versions]
% 2015/07/23 - fixed bug for output fit.meta_ca and fit.S1units.meta_c1. 
%            - added comments to help section as well as a warning output 
%              for nR_S1 or nR_S2 inputs containing zeros
% 2014/10/14 - updated discussion of "s" input in the help section above.
% 2010/09/07 - created
"""

import numpy as np
from scipy.stats import norm
from scipy.optimize import Bounds, LinearConstraint, minimize, SR1

# returns negative log-likelihood of parameters given experimental data
# parameters[0] = meta d'
# parameters[1:end] = type-2 criteria locations
def fitM_rS1_logL(parameters,inputObj):
    meta_d1_rS1 = parameters[0]
    t2c1        = parameters[1:]
    nR_S1, nR_S2, nRatings, d1, t1c1, s, constant_criterion_rS1, constant_criterion_rS2, fncdf, fninv = inputObj

    # define mean and SD of S1 and S2 distributions
    S1mu = -meta_d1_rS1/2
    S1sd = 1
    S2mu = meta_d1_rS1/2
    S2sd = S1sd/s

    # adjust so that everything is centered on t1c1=0
    h = 1 - norm.cdf(0, S2mu, S2sd)
    f = 1 - norm.cdf(0, S1mu, S2sd)

    # this is the value of c1 midway b/t S1 and S2
    shift_c1 = (-1 / (1 + s)) * (norm.ppf(h) + norm.ppf(f))

    # shift S1 and S2mu so that they lie on an axis for 0 --> c1=0
    S1mu = S1mu + shift_c1
    S2mu = S2mu + shift_c1

    # adjust so that the type 1 criterion is set at 0
    # (this is just to work with optimization toolbox constraints...
    #  to simplify defining the upper and lower bounds of type 2 criteria)
    S1mu = S1mu - eval(constant_criterion_rS1)
    S2mu = S2mu - eval(constant_criterion_rS1)

    t1c1 = 0

    # set up MLE analysis
    # get type 2 response counts
    # S1 responses
    nC_rS1 = [nR_S1[i] for i in range(nRatings)]
    nI_rS1 = [nR_S2[i] for i in range(nRatings)]

    # get type 2 probabilities
    C_area_rS1 = fncdf(t1c1,S1mu,S1sd)
    I_area_rS1 = fncdf(t1c1,S2mu,S2sd)
    
    t2c1x = [-np.inf]
    t2c1x.extend(t2c1)
    t2c1x.append(t1c1)

    prC_rS1 = [( fncdf(t2c1x[i+1],S1mu,S1sd) - fncdf(t2c1x[i],S1mu,S1sd) ) / C_area_rS1 for i in range(nRatings)]
    prI_rS1 = [( fncdf(t2c1x[i+1],S2mu,S2sd) - fncdf(t2c1x[i],S2mu,S2sd) ) / I_area_rS1 for i in range(nRatings)]

    # calculate logL
    logL = np.sum([
            nC_rS1[i]*np.log(prC_rS1[i]) \
            + nI_rS1[i]*np.log(prI_rS1[i]) for i in range(nRatings)])
    
    if np.isinf(logL) or np.isnan(logL):
#        logL=-np.inf
        logL=-1e+300 # returning "-inf" may cause optimize.minimize() to fail
    return -logL

def fitM_rS2_logL(parameters,inputObj):
    meta_d1_rS2 = parameters[0]
    t2c1    = parameters[1:]
    nR_S1, nR_S2, nRatings, d1, t1c1, s, constant_criterion_rS1, constant_criterion_rS2, fncdf, fninv = inputObj

    # define mean and SD of S1 and S2 distributions
    S1mu = -meta_d1_rS2/2
    S1sd = 1
    S2mu = meta_d1_rS2/2
    S2sd = S1sd/s

    # adjust so that everything is centered on t1c1=0
    h = 1 - norm.cdf(0, S2mu, S2sd)
    f = 1 - norm.cdf(0, S1mu, S1sd)

    # this is the value of c1 midway b/t S1 and S2
    shift_c1 = (-1 / (1 + s)) * (norm.ppf(h) + norm.ppf(f))

    # shift S1 and S2mu so that they lie on an axis for 0 --> c1=0
    S1mu = S1mu + shift_c1
    S2mu = S2mu + shift_c1

    # adjust so that the type 1 criterion is set at 0
    # (this is just to work with optimization toolbox constraints...
    #  to simplify defining the upper and lower bounds of type 2 criteria)
    S1mu = S1mu - eval(constant_criterion_rS2)
    S2mu = S2mu - eval(constant_criterion_rS2)

    t1c1 = 0

    # set up MLE analysis
    # get type 2 response counts
    # S2 responses
    nC_rS2 = [nR_S2[i+nRatings] for i in range(nRatings)]
    nI_rS2 = [nR_S1[i+nRatings] for i in range(nRatings)]

    # get type 2 probabilities
    C_area_rS2 = 1-fncdf(t1c1,S2mu,S2sd)
    I_area_rS2 = 1-fncdf(t1c1,S1mu,S1sd)
    
    t2c1x = [t1c1]
    t2c1x.extend(t2c1)
    t2c1x.append(np.inf)

    prC_rS2 = [( (1-fncdf(t2c1x[i],S2mu,S2sd)) - (1-fncdf(t2c1x[i+1],S2mu,S2sd)) ) / C_area_rS2 for i in range(nRatings)]
    prI_rS2 = [( (1-fncdf(t2c1x[i],S1mu,S1sd)) - (1-fncdf(t2c1x[i+1],S1mu,S1sd)) ) / I_area_rS2 for i in range(nRatings)]

    # calculate logL
    logL = np.sum([nC_rS2[i]*np.log(prC_rS2[i]) \
            + nI_rS2[i]*np.log(prI_rS2[i]) for i in range(nRatings)])
    
    if np.isinf(logL) or np.isnan(logL):
#        logL=-np.inf
        logL=-1e+300 # returning "-inf" may cause optimize.minimize() to fail
    return -logL

def SDT_s_convert(in1, s, cont1, cont2):
    # in1 - input var to be converted
    # s - sd(S1)/sd(S2)
    # cont1 - identity of in1
    # cont2 - identity of out
    #
    # cont1 and cont2 can be these tokens
    # 'da','d1','d2','ca','c1', 'c2'

    # convert d'
    # s = d2 / d1
    # da = sqrt(2 / (1 + s**2)) * d2
    if (cont1 == 'da'):
        da = in1
        d2 = da / np.sqrt(2 / ( 1 + s**2))
        d1 = d2 / s
    elif (cont1 == 'd1'):
        d1 = in1
        d2 = d1 * s
        da = np.sqrt(2/ (1 + s**2)) * d2
    elif (cont1 == 'd2'):     
        d2 = in1
        d1 = d2 / s
        da = np.sqrt(2 / (1 + s**2)) * d2        
    # convert c
    # s = c2 / c1
    # ca = ( sqrt(2)*s / sqrt(1 + s**2) ) * c1;
    elif (cont1 == 'ca'):
        ca = in1
        c1 = (np.sqrt(1 + s**2) / np.sqrt(2) * s) * ca
        c2 = c1 * s
    elif (cont1 == 'c1'):
        c1 = in1
        c2 = c1 * s
        ca = (np.sqrt(2) * s / np.sqrt(1 + s**2) ) * c1
    elif (cont1 == 'c2'):
        c2 = in1
        c1 = c2 / s
        ca = (np.sqrt(2) * s / np.sqrt(1 + s**2) ) * c1

    return eval(cont2)

def fit_rs_meta_d_MLE(nR_S1, nR_S2, s = 1, fncdf = norm.cdf, fninv = norm.ppf):

    # check inputs
    if (len(nR_S1) % 2)!=0: 
        raise('input arrays must have an even number of elements')
    if len(nR_S1)!=len(nR_S2):
        raise('input arrays must have the same number of elements')
    if any(np.array(nR_S1) == 0) or any(np.array(nR_S2) == 0):
        print(' ')
        print('WARNING!!')
        print('---------')
        print('Your inputs')
        print(' ')
        print('nR_S1:')
        print(nR_S1)
        print('nR_S2:')
        print(nR_S2)
        print(' ')
        print('contain zeros! This may interfere with proper estimation of meta-d''.')
        print('See ''help fit_meta_d_MLE'' for more information.')
        print(' ')
        print(' ')
    
    nRatings = int(len(nR_S1) / 2)  # number of ratings in the experiment
    nCriteria = int(2*nRatings - 1) # number criteria to be fitted
    
    # find actual type 2 FAR and HR (data to be fit)
    # I_nR and C_nR are rating trail counts for incorrect and correct trials
    I_nR_rS2 = nR_S1[nRatings:]
    I_nR_rS1 = (nR_S2[:nRatings])[::-1]

    C_nR_rS2 = nR_S2[nRatings:]
    C_nR_rS1 = (nR_S1[:nRatings])[::-1]

    t2FAR_rS2 = []
    t2HR_rS2  = []
    t2FAR_rS1 = []
    t2HR_rS1  = []
    for i in range(1, nRatings):
        t2FAR_rS2.append(sum(I_nR_rS2[i:]) / sum(I_nR_rS2))
        t2HR_rS2.append(sum(C_nR_rS2[i:]) / sum(C_nR_rS2))

        t2FAR_rS1.append(sum(I_nR_rS1[i:]) / sum(I_nR_rS1))
        t2HR_rS1.append(sum(C_nR_rS1[i:]) / sum(C_nR_rS1))

    """
    set up constraints for scipy.optimize.minimum()
    """
    # parameters
    # meta-d' - 1
    # t2c     - (nCriteria - 1) / 2

    A = []
    ub = []
    lb = []

    nCritPerResp = (nCriteria - 1) // 2

    for crit in range(nCritPerResp - 1):
        # c(crit) <= c(crit+1) --> c(crit) - c(crit+1) <= .001
        tempArow = []
        tempArow.extend(np.zeros(crit+1))
        tempArow.extend([1, -1])
        tempArow.extend(np.zeros((nCritPerResp-1)-crit-1))
        A.append(tempArow)
        ub.append(-.001)
        lb.append(-np.inf)
        
    LB_rS1 = []
    LB_rS1.append(-10.)                          # meta-d' rS1
    LB_rS1.extend(-20 * np.ones(nCritPerResp))   # criteria lower than t1c
    
    UB_rS1 = []
    UB_rS1.append(10.)                      # meta-d' rS1
    UB_rS1.extend(np.zeros(nCritPerResp))   # criteria lower than t1c

    LB_rS2 = []
    LB_rS2.append(-10.)                     # meta-d' rS2
    LB_rS2.extend(np.zeros(nCritPerResp))   # criteria higher than t1c
    
    UB_rS2 = []
    UB_rS2.append(10.)                      # meta-d' rS2
    UB_rS2.extend(20 * np.ones(nCritPerResp))   # criteria higher than t1c
    
    """
    prepare other inputs for scipy.optimize.minimum()
    """
    # select constant criterion type
    constant_criterion_rS1 = 'meta_d1_rS1 * (t1c1 / d1)' # relative criterion
    constant_criterion_rS2 = 'meta_d1_rS2 * (t1c1 / d1)' # relative criterion
    
    # set up initial guess at parameter values
    ratingHR  = []
    ratingFAR = []
    for c in range(1, int(nRatings * 2)):
        ratingHR.append(sum(nR_S2[c:]) / sum(nR_S2))
        ratingFAR.append(sum(nR_S1[c:]) / sum(nR_S1))
    
    # obtain index in the criteria array to mark Type I and Type II criteria
    t1_index = nRatings-1
    t2_index = list(set(list(range(0, 2 * nRatings - 1))) - set([t1_index]))
    
    d1 = (1 / s) * fninv( ratingHR[t1_index] ) - fninv( ratingFAR[t1_index] )
    meta_d1_rS1 = d1
    meta_d1_rS2 = d1
    
    c1 = (-1 / (1 + s)) * ( fninv( ratingHR ) + fninv( ratingFAR ) )
    t1c1 = c1[t1_index]
    t2c1 = c1[t2_index]
    
    # initial values for the minimization function
    guess_rS1 = [meta_d1_rS1]
    guess_rS1.extend(list(t2c1[:nCritPerResp] - eval(constant_criterion_rS1)))
    guess_rS2 = [meta_d1_rS2]
    guess_rS2.extend(list(t2c1[nCritPerResp:] - eval(constant_criterion_rS2)))
    
    # other inputs for the minimization function
    inputObj_rS1 = [nR_S1, nR_S2, nRatings, d1, t1c1, s, constant_criterion_rS1, constant_criterion_rS2, fncdf, fninv]        
    bounds_rS1 = Bounds(LB_rS1, UB_rS1)
    linear_constraint = LinearConstraint(A, lb, ub)
    
    # minimization of negative log-likelihood for rS1
    results = minimize(fitM_rS1_logL, guess_rS1, args = (inputObj_rS1), method='trust-constr',
                       jac='2-point', hess=SR1(),
                       constraints = [linear_constraint],
                       options = {'verbose': 1}, bounds = bounds_rS1)
    
    # quickly process some of the output
    meta_d1_rS1 = results.x[0]
    meta_c1_rS1 = eval(constant_criterion_rS1)

    t2c1_rS1    = results.x[1:] + eval(constant_criterion_rS1)
    logL_rS1    = -results.fun
    
    
    ## find model-estimated type 2 FAR and HR for S1 respones

    # find the estimated type 2 FAR and HR
    S1mu = -meta_d1_rS1 / 2
    S1sd = 1
    S2mu = meta_d1_rS1 / 2
    S2sd = S1sd / s

    # adjust so that everything is centered on t1c1 = 0
    h = 1 - norm.cdf(0, S2mu, S2sd)
    f = 1 - norm.cdf(0, S1mu, S1sd)

    # this is the value of c1 midway b/t S1 and S2
    shift_c1 = (-1 / (1 + s)) * (norm.ppf(h) + norm.ppf(f)) 

    # shift S1 and S2mu so that they lie on an axis for 0 --> c1=0
    S1mu = S1mu + shift_c1
    S2mu = S2mu + shift_c1

    C_area_rS1 = fncdf(meta_c1_rS1, S1mu, S1sd)
    I_area_rS1 = fncdf(meta_c1_rS1, S2mu, S2sd)

    est_t2FAR_rS1 = []
    est_t2HR_rS1 = []

    for i in range(len(t2c1_rS1)):
        t2c1_lower = t2c1_rS1[i]

        I_FAR_area_rS1 = fncdf(t2c1_lower, S2mu, S2sd)
        C_HR_area_rS1  = fncdf(t2c1_lower, S1mu, S1sd)

        est_t2FAR_rS1.append(I_FAR_area_rS1 / I_area_rS1)
        est_t2HR_rS1.append(C_HR_area_rS1 / C_area_rS1)

    ## fit for S2 responses

    # find the best fit for type 2 hits and FAs
    inputObj_rS2 = [nR_S1, nR_S2, nRatings, d1, t1c1, s, constant_criterion_rS1, constant_criterion_rS2, fncdf, fninv]        
    bounds_rS2 = Bounds(LB_rS2, UB_rS2)
    linear_constraint = LinearConstraint(A, lb, ub)

    # minimization of negative log-likelihood for rS2
    results = minimize(fitM_rS2_logL, guess_rS2, args = (inputObj_rS2), method='trust-constr',
                       jac='2-point', hess=SR1(),
                       constraints = [linear_constraint],
                       options = {'verbose': 1}, bounds = bounds_rS2)

    # quickly process some of the output
    meta_d1_rS2 = results.x[0]
    meta_c1_rS2 = eval(constant_criterion_rS2)

    t2c1_rS2    = results.x[1:] + eval(constant_criterion_rS2)
    logL_rS2    = -results.fun

    ## find the estimated type 2 FAR and HR

    S1mu = -meta_d1_rS2 / 2
    S1sd = 1
    S2mu = meta_d1_rS2 / 2
    S2sd = S1sd / s

    # adjust so that everything is centered on t1c1 = 0
    h = 1 - norm.cdf(0, S2mu, S2sd)
    f = 1 - norm.cdf(0, S1mu, S1sd)

    # this is the value of c1 midway b/t S1 and S2
    shift_c1 = (-1 / (1 + s)) * (norm.ppf(h) + norm.ppf(f)) 

    # shift S1 and S2mu so that they lie on an axis for 0 --> c1=0
    S1mu = S1mu + shift_c1
    S2mu = S2mu + shift_c1

    C_area_rS2 = fncdf(meta_c1_rS2, S1mu, S1sd)
    I_area_rS2 = fncdf(meta_c1_rS2, S2mu, S2sd)

    est_t2FAR_rS2 = []
    est_t2HR_rS2 = []

    for i in range(len(t2c1_rS2)):
        t2c1_upper = t2c1_rS2[i]

        I_FAR_area_rS2 = 1 - fncdf(t2c1_upper, S2mu, S2sd)
        C_HR_area_rS2  = 1 - fncdf(t2c1_upper, S1mu, S1sd)

        est_t2FAR_rS2.append(I_FAR_area_rS2 / I_area_rS2)
        est_t2HR_rS2.append(C_HR_area_rS2 / C_area_rS2)

    ## package output

    # type 1 params
    fit = {}
    fit['da']       = SDT_s_convert(d1, s, 'd1', 'da')
    fit['t1ca']     = SDT_s_convert(t1c1, s, 'c1', 'ca')
    fit['s']        = s
    
    # type 2 fits for rS1
    fit['meta_da_rS1']  = SDT_s_convert(meta_d1_rS1, s,'d1','da')
    fit['t1ca_rS1']     = SDT_s_convert(meta_c1_rS1, s,'c1','ca')
    fit['t2ca_rS1']     = SDT_s_convert(t2c1_rS1, s,'c1','ca')
    
    fit['M_ratio_rS1']  = fit['meta_da_rS1'] / fit['da']
    fit['M_diff_rS1']   = fit['meta_da_rS1'] - fit['da']

    fit['logL_rS1']     = logL_rS1

    fit['obs_HR2_rS1']  = t2HR_rS1
    fit['est_HR2_rS1']  = est_t2HR_rS1
    fit['obs_FAR2_rS1'] = t2FAR_rS1
    fit['est_FAR2_rS1'] = est_t2FAR_rS1

    # type 2 fits for rS2
    fit['meta_da_rS2']  = SDT_s_convert(meta_d1_rS2, s,'d1','da')
    fit['t1ca_rS2']     = SDT_s_convert(meta_c1_rS2, s,'c1','ca')
    fit['t2ca_rS2']     = SDT_s_convert(t2c1_rS2, s,'c1','ca')
    
    fit['M_ratio_rS2']  = fit['meta_da_rS2'] / fit['da']
    fit['M_diff_rS2']   = fit['meta_da_rS2'] - fit['da']

    fit['logL_rS2']     = logL_rS2

    fit['obs_HR2_rS2']  = t2HR_rS2
    fit['est_HR2_rS2']  = est_t2HR_rS2
    fit['obs_FAR2_rS2'] = t2FAR_rS2
    fit['est_FAR2_rS2'] = est_t2FAR_rS2
    
    # S1 units
    fit['S1units'] = {}
    fit['S1units']['d1']        = d1
    fit['S1units']['t1c1']      = t1c1

    fit['S1units']['meta_d1_rS1']   = meta_d1_rS1
    fit['S1units']['t1c1_rS1']      = meta_c1_rS1
    fit['S1units']['t2c1_rS1']      = t2c1_rS1

    fit['S1units']['meta_d1_rS2']   = meta_d1_rS2
    fit['S1units']['t1c1_rS2']      = meta_c1_rS2
    fit['S1units']['t2c1_rS2']      = t2c1_rS2

    return fit

"""
# try using the function
nR_S1 = [36, 24, 17, 20, 10, 12, 9, 2]
nR_S2 = [1, 4, 10, 11, 19, 18, 28, 39]

fit = fit_rs_meta_d_MLE(nR_S1,nR_S2)
"""
