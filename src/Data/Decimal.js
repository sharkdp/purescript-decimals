var Decimal = require("decimal.js");

Decimal.set({ precision: 30 });

exports.fromInt = function(x) {
  return new Decimal(x);
};

exports["fromString'"] = function(nothing) {
  return function(just) {
    return function(str) {
      try {
        return just(new Decimal(str));
      }
      catch (e) {
        return nothing;
      }
    };
  };
};

exports.fromNumber = function(x) {
  return new Decimal(x);
};

exports.toNumber = function(x) {
  return x.toNumber();
};

exports.toString = function(x) {
  return x.toString();
};

exports.toPrecision = function(d) {
  return function (x) {
    return x.toPrecision(d);
  };
};

exports.isInteger = function(x) {
  return x.isInteger();
};

exports.isFinite = function(x) {
  return x.isFinite();
};

exports.toSignificantDigits = function(d) {
  return function (x) {
    return x.toSignificantDigits(d);
  };
};

exports.dAdd = function(x) {
  return function(y) {
    return x.add(y);
  };
};

exports.dMul = function(x) {
  return function(y) {
    return x.mul(y);
  };
};

exports.dSub = function(x) {
  return function(y) {
    return x.minus(y);
  };
};

exports.dDiv = function(x) {
  return function(y) {
    return x.div(y);
  };
};

exports.dEquals = function(x) {
  return function(y) {
    return x.equals(y);
  };
};

exports.dCompare = function(x) {
  return function(y) {
    return x.cmp(y);
  };
};

exports.abs = function(x) {
  return x.abs();
};

exports.pow = function(x) {
  return function(y) {
    return x.pow(y);
  };
};

exports.exp = function(x) {
  return x.exp();
};

exports.acos = function(x) {
  return x.acos();
};

exports.abs = function(x) {
  return x.abs();
};

exports.acos = function(x) {
  return x.acos();
};

exports.acosh = function(x) {
  return x.acosh();
};

exports.asin = function(x) {
  return x.asin();
};

exports.asinh = function(x) {
  return x.asinh();
};

exports.atan = function(x) {
  return x.atan();
};

exports.atanh = function(x) {
  return x.atanh();
};

exports.atan2 = function(x) {
  return function(y) {
    return Decimal.atan2(x, y);
  };
};

exports.ceil = function(x) {
  return x.ceil();
};

exports.cos = function(x) {
  return x.cos();
};

exports.cosh = function(x) {
  return x.cosh();
};

exports.exp = function(x) {
  return x.exp();
};

exports.floor = function(x) {
  return x.floor();
};

exports.ln = function(x) {
  return x.ln();
};

exports.log2 = function(x) {
  return Decimal.log2(x);
};

exports.log10 = function(x) {
  return Decimal.log10(x);
};

exports.max = function(x) {
  return function(y) {
    return Decimal.max(x, y);
  };
};

exports.min = function(x) {
  return function(y) {
    return Decimal.min(x, y);
  };
};

exports.round = function(x) {
  return x.round();
};

exports.sin = function(x) {
  return x.sin();
};

exports.sinh = function(x) {
  return x.sinh();
};

exports.sqrt = function(x) {
  return x.sqrt();
};

exports.tan = function(x) {
  return x.tan();
};

exports.tanh = function(x) {
  return x.tanh();
};

exports.e = Decimal.exp(1.0);

exports.pi = new Decimal('3.14159265358979323846264338327950288419716939937510582097494459230781640628620899');
