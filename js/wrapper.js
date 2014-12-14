
function reactWrapCallback (cb) {
  return function () {
    // capture proper this in order to access component api
    var that = this;
    var obj = {};
    var result = cb(that, obj);
    if (result === null) {
      return obj.result;
    }
    else {
      throw "Operation blocked";
    }
  };
}

