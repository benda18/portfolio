library(dplyr)
library(renv)
library(openssl)
library(sodium)



snapshot(exclude = "rgdal")

rm(list=ls());cat('\f')
gc()

testmsg <- "foo"
testkey <- NULL



# play----

# openssl::blake2b(x   = testmsg, key = testkey)
# openssl::blake2s(x   = testmsg, key = testkey)
# openssl::keccak(x    = testmsg, key = testkey, size = 224)
# openssl::md4(x       = testmsg, key = testkey)
openssl::md5(x       = testmsg, key = testkey)
# openssl::ripemd160(x = testmsg, key = testkey)
# openssl::sha1(x      = testmsg, key = testkey)
# openssl::sha2(x      = testmsg, key = testkey)
# openssl::sha224(x    = testmsg, key = testkey)
# openssl::sha256(x    = testmsg, key = testkey)
# openssl::sha3(x      = testmsg, key = testkey)
# openssl::sha384(x    = testmsg, key = testkey)
# openssl::sha512(x    = testmsg, key = testkey)



# hash_simple <- function(key_seq = "12345"){
#   
#   # convert to character
#   if(is.numeric(key_seq)){
#     key_seq <- as.character(key_seq)
#   }
#   
#   # 5 or 6 pin key? 
#   n_pins <- nchar(key_seq)
#   
#   
#   data.frame(key_pins)
#   
# }

hash_kwikset <- function(key_seq = "123456", 
                         hash_key = "foo",
                         n_pins, 
                         max_keyval = 7, 
                         min_keyval = 0){
  require(dplyr)
  require(openssl)
  
  # convert to character
  if(is.numeric(key_seq)){
    key_seq <- as.character(key_seq)
  }
  # convert to numeric vector
  key_vec <- as.numeric(unlist(strsplit(key_seq,"")))
  # kwikset typically has values from 0-7. confirm:
  stopifnot(key_vec >= min_keyval,
            key_vec <= max_keyval,
            # check that input pins matches anticipated
            nchar(key_seq) == n_pins)
  
  openssl::md5(x   = key_seq, 
               key = hash_key)
 
  
}

hash_kwikset("123456", hash_key = "foo", n_pins = 6)

md5("1")
