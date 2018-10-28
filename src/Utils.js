'use strict';

exports.urlEncodedImpl = function(makeTuple){
  return function(obj){
    return Object.keys(obj).map(function(k) {
      return makeTuple(k)(obj[k].toString());
    });
  };
};


exports.coerceJsonHelperImpl = function(obj){

  if(obj == null){
    return obj;
  }
  else if(Array.isArray(obj)){
    var newObj = [];
    for(var i=0;i<obj.length;i++){
      newObj.push(exports.coerceJsonHelperImpl(obj[i]));
    }
    return newObj;
  }
  else if(obj != null && typeof(obj) === 'object'){
    var newObj = {};
    console.log('low', obj, obj === [], obj == []);
    Object.keys(obj).map(function(k) {
      newObj[k.toLowerCase()] = exports.coerceJsonHelperImpl(obj[k]);
    });
    return newObj;
  }
  else{
    return obj;
  }
};
