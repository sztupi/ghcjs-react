
function reactWrapCallback (cb) {
  return function () {
    var obj = {};
    var result = cb(obj);
    if (result === null) {
      return obj.result;
    }
    else {
      throw "Operation blocked";
    }
  };
}

