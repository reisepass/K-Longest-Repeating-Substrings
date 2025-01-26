#include <Rcpp.h>
#include <vector>
#include <iostream>
#include <string>
#include <sstream>
#include <iterator>
#include <tuple>
#include <unordered_map>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}



// trim from start (in place)
static inline void ltrim(std::string &s) {
  s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](int ch) {
    return !std::isspace(ch);
  }));
}

// trim from end (in place)
static inline void rtrim(std::string &s) {
  s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) {
    return !std::isspace(ch);
  }).base(), s.end());
}

// trim from both ends (in place)
static inline void trim(std::string &s) {
  ltrim(s);
  rtrim(s);
}

// trim from start (copying)
static inline std::string ltrim_copy(std::string s) {
  ltrim(s);
  return s;
}

// trim from end (copying)
static inline std::string rtrim_copy(std::string s) {
  rtrim(s);
  return s;
}

// trim from both ends (copying)
static inline std::string trim_copy(std::string s) {
  trim(s);
  return s;
}

template<typename Out>
void split(const std::string &s, char delim, Out result) {
  std::stringstream ss(s);
  std::string item;
  while (std::getline(ss, item, delim)) {
    *(result++) = item;
  }
}

std::vector<std::string> split(const std::string &s, char delim) {
  std::vector<std::string> elems;
  split(s, delim, std::back_inserter(elems));
  return elems;
}




// [[Rcpp::export]]
List cpp_str_split( std::vector< std::string > documents, char delim ) {

  int num_docs = documents.size();

  std::vector<std::vector<std::string> > sentStrs;
  std::vector<std::vector<int> > sentLenghts ;
    for( int i=0; i < num_docs; i++ ) {


      std::vector<std::string> sents = split(documents[i], delim);
      std::vector<int> lens(sents.size());
      for( int j=0; j < sents.size(); j++ ) {
        lens[j]=sents[j].length();

      }
      sentLenghts.push_back( lens );
      sentStrs.push_back(sents);
    }
     List out;
    out.push_back(sentStrs);
    out.push_back(sentLenghts);
  return out;
}



unsigned int
  mini_hash(
    const char* s,
    unsigned int seed = 0)
  {
    unsigned int hash = seed;
    while (*s)
    {
      hash = hash * 101  +  *s++;
    }
    return hash;
  }


// [[Rcpp::export]]
List cpp_str_split_and_hash_window_of_past_splits( std::vector< std::string > documents, char delim, int window_width ) {

  int num_docs = documents.size();

  std::vector<std::vector<std::string> > sentStrs;
  std::vector<std::vector<unsigned int> > sentWindowHash ;
  for( int i=0; i < num_docs; i++ ) {


    std::vector<std::string> sents = split(documents[i], delim);
    std::vector<unsigned int> hashes(sents.size());
    for( int j=0; j < sents.size(); j++ ) {
      if(j<window_width){
        hashes[j]=0;
      }
      else{
        std::stringstream ss;
        for( int nj=j; nj>=j-window_width;nj--){
          ss<<sents[nj];
        }
        hashes[j]=mini_hash(ss.str().c_str());
      }

    }
    sentWindowHash.push_back( hashes );
    sentStrs.push_back(sents);
  }
  List out;
  out.push_back(sentStrs);
  out.push_back(sentWindowHash);
  return out;
}


// [[Rcpp::export]]
List cpp_str_split_and_concat_docs_and_hash_window_of_past_splits( std::vector< std::string > documents, char delim, int window_width ) {

  std::string oneBigDoc ;
  //oneBigDoc = accumulate(begin(documents),end(documents),oneBigDoc);

  // TODO add end of document special sentence  //In the case of hacker news there is always a <p> at the end but I will want filter HTML stuff out soon
  for (const auto &piece : documents) oneBigDoc += piece;




    std::vector<std::string> sents = split(oneBigDoc, delim);
    std::vector<unsigned int> hashes(sents.size());
    for( int j=0; j < sents.size(); j++ ) {
      if(j<window_width){
        hashes[j]=0;
      }
      else{
        std::stringstream ss;
        for( int nj=j; nj>=j-window_width;nj--){
          ss<<sents[nj];
        }
        hashes[j]=mini_hash(ss.str().c_str());
      }


   }
  List out;
  out.push_back(sents);
  out.push_back(hashes);
  return out;
}


// [[Rcpp::export]]
List cpp_str_split_and_concat_docs_and_hash_window_of_past_splits_and_hashbucket_them( std::vector< std::string > documents, char delim, int window_width, int min_occurance, int minSentLength=3 , bool printSents=false ) {

  std::string oneBigDoc ;
  //oneBigDoc = accumulate(begin(documents),end(documents),oneBigDoc);

  // TODO add end of document special sentence  //In the case of hacker news there is always a <p> at the end but I will want filter HTML stuff out soon
  for (const auto &piece : documents) oneBigDoc += piece;




  std::vector<std::string> sents = split(oneBigDoc, delim);
  //std::remove_if (sents.begin(), sents.end(),  [minSentLength](std::string sentI){return sentI.length()<=minSentLength;} );
  //Lets filter out sentences which are too small.
  //TODO debug this IDK why but i am sitll getting empty sentences  maybe remove_if does not change the indexes but just removes the data
  // Probably i should try a copy if and see what the size is
  std::cout<<" oneBigDoc.length()="<<oneBigDoc.length()<<"\n";
  std::cout<<" sents.size()="<<sents.size()<<"\n";
  std::vector<unsigned int> hashes(sents.size());
  std::unordered_map<unsigned int, unsigned int> hash_sent_loc;
  std::unordered_map<unsigned int, unsigned int> hash_count;

  for( int j=0; j < sents.size(); j++ ) {
    if(j<window_width){
      hashes[j]=0;
    }
    else{
      std::stringstream ss;
      for( int nj=j; nj>=j-window_width;nj--){
        ss<<sents[nj];
      }
      auto h=mini_hash(ss.str().c_str());
      hashes[j]  = h;
      hash_sent_loc[h] = j;
      hash_count[h]++;
    }


  }

  std::vector< int> out_hashCounts;
  std::vector<std::string> out_CountedSents;
  //Printing Sentences with counts
  for (auto it : hash_count) {
    if(it.second>=min_occurance){
      if(printSents)
        std::cout << " Count[" << it.second <<"] " ;
      std::stringstream ssOUT;
        for( int nj=hash_sent_loc[it.first]; nj>=hash_sent_loc[it.first]-window_width;nj--){
          ssOUT<<sents[nj]<<".  ";
        }
      if(printSents)
        std::cout << ssOUT.str() << "\n ";
      out_CountedSents.push_back(ssOUT.str());
      out_hashCounts.push_back(it.second);


    }
}




  List out;
  out.push_back(sents);
  out.push_back(hashes);
  out.push_back(out_hashCounts);
  out.push_back(out_CountedSents);
  return out;
}




//THIS is super  slower
// [[Rcpp::export]]
List cpp_str_splitLL( std::vector< std::string > documents, char delim ) {

  int num_docs = documents.size();

  List sentStrs;
  List sentLenghts ;
  for( int i=0; i < num_docs; i++ ) {


    std::vector<std::string> sents = split(documents[i], delim);
    std::vector<int> lens(sents.size());
    for( int j=0; j < sents.size(); j++ ) {
      lens[j]=sents[j].length();

    }
    sentLenghts.push_back( lens );
    sentStrs.push_back(sents);
  }
  List out(2);
  out.push_back(sentStrs);
  out.push_back(sentLenghts);
  return out;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
timesTwo(42)
*/




// [[Rcpp::export]]
List cpp_countWordsWhichContain( std::vector< std::string > documents, char delim, std::string containsOneOf ) {

  std::string oneBigDoc ;
  for (const auto &piece : documents) oneBigDoc += piece;

  std::vector<std::string> words = split(oneBigDoc, delim);



  std::cout<<" oneBigDoc.length()="<<oneBigDoc.length()<<"\n";
  std::cout<<" words.size()="<<words.size()<<"\n";


  std::unordered_map<std::string , unsigned int> hash_count;

  for(int i=0; i<words.size() ; i++){
    trim(words[i]);
    if(words[i].find_first_of(containsOneOf)!=std::string::npos)
        hash_count[words[i]]++;

  }



  List out;
  out.push_back(hash_count);
  return out;
}



// [[Rcpp::export]]
List cpp_countWords( std::vector< std::string > documents, char delim ) {
  
  std::string oneBigDoc ;
  for (const auto &piece : documents) oneBigDoc += piece;
  
  std::vector<std::string> words = split(oneBigDoc, delim);
  
  
  
  std::cout<<" oneBigDoc.length()="<<oneBigDoc.length()<<"\n";
  std::cout<<" words.size()="<<words.size()<<"\n";
  
  
  std::unordered_map<std::string , unsigned int> hash_count;
  
  for(int i=0; i<words.size() ; i++){
    trim(words[i]);
     
      hash_count[words[i]]++;
    
  }
  
  
  
  List out;
  out.push_back(hash_count);
  return out;
}


