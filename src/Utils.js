'use strict';

exports.urlEncodedImpl = function(makeTuple){
  return function(obj){
    return Object.keys(obj)
      .filter(function(k){return obj[k] != null;})
      .map(function(k) {
          if(Array.isArray(obj[k])){
            return makeTuple(k+"[]")(obj[k].toString());
          }
          else{
            return makeTuple(k)(obj[k].toString());
          }
      });
  };
};


exports.coerceJsonHelperImpl = function(obj, lowcase){
  if(obj == null){
    return obj;
  }
  else if(Array.isArray(obj)){
    var newObj = [];
    for(var i=0;i<obj.length;i++){
      newObj.push(exports.coerceJsonHelperImpl(obj[i], true));
    }
    return newObj;
  }
  else if(obj != null && typeof(obj) === 'object'){
    var newObj = {};
    Object.keys(obj).map(function(k) {
      if(lowcase){
        newObj[k.toLowerCase()] = exports.coerceJsonHelperImpl(obj[k], true);
      }
      else{
        newObj[k] = exports.coerceJsonHelperImpl(obj[k], true);
      }
    });
    return newObj;
  }
  else{
    return obj;
  }
};
