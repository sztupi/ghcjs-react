
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

function provideThis (cb) {
  return function () {
    var that = this;
    var args = Array.prototype.slice.call(arguments, 0);
    return cb.apply(null, [that].concat(args));
  }
}

function provideThisArb (cb) {
  return function () {
    var that = this;
    var args = Array.prototype.slice.call(arguments, 0);
    return cb.call(null, that, args);
  }
}

// wrap shouldComponentRender
function provideThisArbWithResult (cb) {
  return function () {
    var that = this;
    var args = Array.prototype.slice.call(arguments, 0);
    var obj = {};
    var result = cb.call(null, that, [obj].concat(args));
    if (result === null) {
      return obj.result;
    }
    else {
      throw "Operation blocked";
    }
  }
}

