'use strict';

exports.urlEncodedImpl = function(makeTuple){
  return function(obj){
    return Object.keys(obj).map(function(k) {
      return makeTuple(k)(obj[k].toString());
    });
  };
};
