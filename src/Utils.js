'use strict';

exports.urlEncodedImpl = function(makeTuple){
  return function(obj){
    return Object.keys(obj).map(function(k) {
      return makeTuple(k)(obj[k].toString());
    });
  };
};


exports.coerceJsonHelperImpl = function(obj){

  if(obj != null && typeof(obj) == 'object'){
    var newObj = {};
    Object.keys(obj).map(function(k) {
      newObj[k.toLowerCase()] = exports.coerceJsonHelperImpl(obj[k]);
    });
    return newObj;
  }
  else{
    return obj;
  }
};
