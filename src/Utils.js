'use strict';

exports.jsonGetParams = function(obj){
  return Object.keys(obj).map(function(k) {
    return encodeURIComponent(k) + '=' + encodeURIComponent(data[k]);
  }).join('&');
};
