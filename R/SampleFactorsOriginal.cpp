#include <stdlib.h>            // malloc
#include <stdio.h>             // printf
#include <math.h>              // fabs, sqrt, etc.
#include <Rmath.h>              // fabs, sqrt, etc.
#include <time.h>              // time
#include <cmath>
#include <unistd.h>            // getpid
#include <string>
#include <algorithm>
#include <RcppArmadillo.h>
//#include <Rcpp.h>
using namespace Rcpp;
using namespace R;
using namespace arma;



// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec cpp_mvrnorm(arma::vec mu, arma::mat sigma){
  int ncols = sigma.n_cols;
  arma::vec Y = rnorm(ncols,0,1);
  return mu + chol(sigma) * Y;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat sampleFactors(arma::mat Lambda, arma::mat Pmatprime, arma::vec FLambdalong, arma::mat Fmat, arma::vec y, arma::vec mulong, arma::vec Tfullbetalong, arma::vec Bfullxilong, arma::mat Omega, arma::mat SigmaFinv, double sigma2, int ntimes){

  for(int tt=1; tt<=ntimes-2; tt++){

    // Attempt to speed up by using full M.mat
    // Actually, this made it slower.
    // arma::uvec col_sequence;
    // for (int i=tt; i<=ntimes*nfactors; i+= ntimes) {
    //   col_sequence.insert_rows(col_sequence.n_rows, 1);
    //   col_sequence(col_sequence.n_rows - 1) = i;
    // }
    //
    // arma::mat Mt = M(all_rows, col_sequence);
    // arma::mat Mtt = Mt.t();

    int Pmatprimenrows = Pmatprime.n_rows;

    arma::mat Pmatsub = Pmatprime.submat(0,tt,Pmatprimenrows-1, tt);
    arma::mat Pmatsubt = Pmatsub.t();
    arma::mat Lambdat = Lambda.t();

    // "M" allows us to simplify computation inside of Fvar;
    // (L kron P) * (Lt kron Pt) = (LLt kron PPt)
    // We still use Mt and Mtt below in later functions

    arma::mat M = kron(Lambdat*Lambda, Pmatsubt*Pmatsub);
    arma::mat Mt = kron(Lambda, Pmatsub);
    arma::mat Mtt = Mt.t();

    arma::mat Omegat = Omega.t();
    int Fmatcols = Fmat.n_cols;

    arma::vec Fmatsubt = Fmat.submat(tt,0,tt, Fmatcols-1).t();
    arma::vec Fmatsubtm1 = Fmat.submat(tt-1,0,tt-1, Fmatcols-1).t();
    arma::vec Fmatsubtp1 = Fmat.submat(tt+1,0,tt+1, Fmatcols-1).t();

    arma::vec FLambdalongnott = FLambdalong - (Mt*Fmatsubt);
    arma::vec tempt = y - mulong - Tfullbetalong - Bfullxilong - FLambdalongnott;

    arma::mat Fvar = inv(SigmaFinv + Omegat*SigmaFinv*Omega + (1/sigma2)*M);

    arma::vec Fmean = Fvar*(SigmaFinv*Omega*Fmatsubtm1 + Omegat*SigmaFinv*Fmatsubtp1 + (1/sigma2)*Mtt*tempt);

    Fmatsubt = cpp_mvrnorm(Fmean, Fvar);
    Fmat.submat(tt,0,tt, Fmatcols-1) = Fmatsubt.t();
    FLambdalong = FLambdalongnott + Mt*Fmatsubt;

  }

  return Fmat;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat sampleFactorsNoConf(arma::mat Lambda, arma::vec tempvals, arma::mat Fmat, arma::mat Fvar, arma::mat Omega, arma::mat SigmaFinv, double sigma2, int ntimes, int nlocs){

  for(int tt=1; tt<=ntimes-2; tt++){
    arma::uvec ttseq(nlocs);
    for(int jj=1; jj<=nlocs; jj++){
      ttseq[jj-1] = tt + ntimes*(jj-1);
    }
    arma::vec temptt = tempvals.elem(ttseq);
    arma::mat Omegat = Omega.t();
    arma::mat Lambdat = Lambda.t();
    int Fmatcols = Fmat.n_cols;
    arma::vec Fmatsubtm1 = Fmat.submat(tt-1,0,tt-1, Fmatcols-1).t();
    arma::vec Fmatsubtp1 = Fmat.submat(tt+1,0,tt+1, Fmatcols-1).t();

    arma::vec Fmean = Fvar*(SigmaFinv*Omega*Fmatsubtm1 + Omegat*SigmaFinv*Fmatsubtp1 + (1/sigma2)*Lambdat*temptt);

    arma::vec Fmatsubt = cpp_mvrnorm(Fmean, Fvar);
    Fmat.submat(tt,0,tt, Fmatcols-1) = Fmatsubt.t();
  }
  return Fmat;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat submatTest (arma::mat X, arma::uvec ind) {

  return X.cols(ind);
}

// // [[Rcpp::depends(RcppArmadillo)]]
// // [[Rcpp::export]]
// arma::mat sampleAlphaXi(int nbases, arma::vec alphaxi, arma::vec xi, double tau2gamma, double tau2phi, arma::mat newS, arma::vec tau2xi, arma::mat Aprec){
//  int seqsize = alphaxi.size()/nbases;
//  int xiseqsize = xi.size()/nbases;
//  for(int xx=0; xx<=(nbases-1); xx++){
//    arma::uvec xxseq(seqsize);
//    arma::uvec xixxseq(xiseqsize);
//    for(int jj=1; jj<=seqsize; jj++){
//      xxseq[jj-1] = xx + nbases*(jj-1);
//    }
//    for(int kk=1; kk<=xiseqsize; kk++){
//      xixxseq[kk-1] = xx + nbases*(kk-1);
//    }
//    arma::vec alphathis = alphaxi.elem(xxseq);
//    arma::vec xithis = xi.elem(xixxseq);
//
//    double tau2shape = tau2gamma + xiseqsize/2;
//    arma::vec tempthis = xithis - newS*alphathis;
//    arma::mat tau2scale = tau2phi+ 0.5*tempthis.t()*tempthis;
//    double tau2inv = rgamma(1, tau2shape, 1/tau2scale)[0]; // (1/tau2scale) is the scale for the gamma
//    tau2xi[xx] = 1/tau2inv;
//
//    arma::mat alphavar = inv( (1/tau2xi[xx])*(newS.t())*newS + Aprec);
//    arma::vec alphamean = alphavar*((1/tau2xi[xx])*(newS.t())*xithis);
//    alphaxi.elem(xxseq) = my_mvrnorm(alphamean, alphavar);
//
//  }
