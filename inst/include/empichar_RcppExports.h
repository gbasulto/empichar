// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#ifndef RCPP_empichar_RCPPEXPORTS_H_GEN_
#define RCPP_empichar_RCPPEXPORTS_H_GEN_

#include <RcppArmadillo.h>
#include <Rcpp.h>

namespace empichar {

    using namespace Rcpp;

    namespace {
        void validateSignature(const char* sig) {
            Rcpp::Function require = Rcpp::Environment::base_env()["require"];
            require("empichar", Rcpp::Named("quietly") = true);
            typedef int(*Ptr_validate)(const char*);
            static Ptr_validate p_validate = (Ptr_validate)
                R_GetCCallable("empichar", "_empichar_RcppExport_validate");
            if (!p_validate(sig)) {
                throw Rcpp::function_not_exported(
                    "C++ function with signature '" + std::string(sig) + "' not found in empichar");
            }
        }
    }

    inline arma::vec ecf_re_cpp(const arma::mat& t, const arma::mat& smp) {
        typedef SEXP(*Ptr_ecf_re_cpp)(SEXP,SEXP);
        static Ptr_ecf_re_cpp p_ecf_re_cpp = NULL;
        if (p_ecf_re_cpp == NULL) {
            validateSignature("arma::vec(*ecf_re_cpp)(const arma::mat&,const arma::mat&)");
            p_ecf_re_cpp = (Ptr_ecf_re_cpp)R_GetCCallable("empichar", "_empichar_ecf_re_cpp");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_ecf_re_cpp(Shield<SEXP>(Rcpp::wrap(t)), Shield<SEXP>(Rcpp::wrap(smp)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<arma::vec >(rcpp_result_gen);
    }

    inline arma::vec ecf_im_cpp(const arma::mat& t, const arma::mat& smp) {
        typedef SEXP(*Ptr_ecf_im_cpp)(SEXP,SEXP);
        static Ptr_ecf_im_cpp p_ecf_im_cpp = NULL;
        if (p_ecf_im_cpp == NULL) {
            validateSignature("arma::vec(*ecf_im_cpp)(const arma::mat&,const arma::mat&)");
            p_ecf_im_cpp = (Ptr_ecf_im_cpp)R_GetCCallable("empichar", "_empichar_ecf_im_cpp");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_ecf_im_cpp(Shield<SEXP>(Rcpp::wrap(t)), Shield<SEXP>(Rcpp::wrap(smp)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<arma::vec >(rcpp_result_gen);
    }

    inline arma::vec ecf_mod_cpp(const arma::mat& t, const arma::mat& smp) {
        typedef SEXP(*Ptr_ecf_mod_cpp)(SEXP,SEXP);
        static Ptr_ecf_mod_cpp p_ecf_mod_cpp = NULL;
        if (p_ecf_mod_cpp == NULL) {
            validateSignature("arma::vec(*ecf_mod_cpp)(const arma::mat&,const arma::mat&)");
            p_ecf_mod_cpp = (Ptr_ecf_mod_cpp)R_GetCCallable("empichar", "_empichar_ecf_mod_cpp");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_ecf_mod_cpp(Shield<SEXP>(Rcpp::wrap(t)), Shield<SEXP>(Rcpp::wrap(smp)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<arma::vec >(rcpp_result_gen);
    }

    inline arma::cx_vec ecf_cpp(const arma::mat& t, const arma::mat& smp) {
        typedef SEXP(*Ptr_ecf_cpp)(SEXP,SEXP);
        static Ptr_ecf_cpp p_ecf_cpp = NULL;
        if (p_ecf_cpp == NULL) {
            validateSignature("arma::cx_vec(*ecf_cpp)(const arma::mat&,const arma::mat&)");
            p_ecf_cpp = (Ptr_ecf_cpp)R_GetCCallable("empichar", "_empichar_ecf_cpp");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_ecf_cpp(Shield<SEXP>(Rcpp::wrap(t)), Shield<SEXP>(Rcpp::wrap(smp)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<arma::cx_vec >(rcpp_result_gen);
    }

}

#endif // RCPP_empichar_RCPPEXPORTS_H_GEN_