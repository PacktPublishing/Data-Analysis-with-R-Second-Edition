#include <Rcpp.h>

// [[Rcpp::export]]
double square(double number){
    return(pow(number, 2));
}

// [[Rcpp::export]]
double to_radians_cpp(double degrees){
    return(degrees * 3.141593 / 180);
}


// [[Rcpp::export]]
double sum2(Rcpp::NumericVector a_vector){
    double running_sum = 0;
    int length = a_vector.size();
    for( int i = 0; i < length; i++ ){
        running_sum = running_sum + a_vector(i);
    }
    return(running_sum);
}



// [[Rcpp::export]]
double haversine_cpp(double lat1, double long1,
                     double lat2, double long2,
                     std::string unit="km"){
    int radius = 6378;
    double delta_phi = to_radians_cpp(lat2 - lat1);
    double delta_lambda = to_radians_cpp(long2 - long1);
    double phi1 = to_radians_cpp(lat1);
    double phi2 = to_radians_cpp(lat2);
    double term1 = pow(sin(delta_phi / 2), 2);
    double term2 = cos(phi1) * cos(phi2) * pow(sin(delta_lambda/2), 2);
    double the_terms = term1 + term2;
    double delta_sigma = 2 * atan2(sqrt(the_terms), sqrt(1-the_terms));
    double distance = radius * delta_sigma;
 
    /* if it is anything *but* km it is miles */
    if(unit != "km"){
        return(distance*0.621371);
    }
 
    return(distance);
}



// [[Rcpp::export]]
double single_core_cpp(Rcpp::NumericMatrix mat){
    int nrows = mat.nrow();
    int numcomps = nrows*(nrows-1)/2;
    double running_sum = 0;
    for( int i = 0; i < nrows; i++ ){
        for( int j = i+1; j < nrows; j++){
            double this_dist = haversine_cpp(mat(i,0), mat(i,1),
                                             mat(j,0), mat(j,1));

            running_sum = running_sum + this_dist;
        }
    }
    return running_sum / numcomps;
}


