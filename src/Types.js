'use strict';

exports.parseReportStructureImpl = function(err){
  return function(succ){
    return function(obj){
      if(obj == null || typeof(obj) != 'object)'){
        return err(obj.toString);
      }
      const foreignArrays = Object.keys(obj).map(function(k) {
        return {cndId: k, regions: obj[k]};
      });
      return succ(foreignArrays);
    };
  };
};
