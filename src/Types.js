'use strict';

exports.parseReportStructureImpl = function(err){
  return function(succ){
    return function(obj){
      if(obj === null){
        return err('null or undefined');
      }
      else if(typeof(obj) !== 'object'){
        return err(obj.toString() + 'cant be parsed as object');
      }
      const foreignArrays = Object.keys(obj).map(function(k) {
        return {cdnId: k, regions: obj[k]};
      });
      return succ(foreignArrays);
    };
  };
};
