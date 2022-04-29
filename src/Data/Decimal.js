import Decimal from "decimal.js";

Decimal.set({ precision: 30 });
Decimal.set({ modulo: Decimal.EUCLID });

export function fromInt(x) {
  return new Decimal(x);
}

export function fromStringImpl(nothing) {
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
}

export function fromNumber(x) {
  return new Decimal(x);
}

export function toNumber(x) {
  return x.toNumber();
}

export function toString(x) {
  return x.toString();
}

export function toPrecision(d) {
  return function (x) {
    return x.toPrecision(d);
  };
}

export function toFixed(d) {
    return function(x) {
        return x.toFixed(d);
    }
}

export function isInteger(x) {
  return x.isInteger();
}

export function isFinite(x) {
  return x.isFinite();
}

export function toSignificantDigits(d) {
  return function (x) {
    return x.toSignificantDigits(d);
  };
}

export function dAdd(x) {
  return function(y) {
    return x.add(y);
  };
}

export function modulo(x) {
  return function(y) {
    return x.mod(y);
  };
}

export function dMul(x) {
  return function(y) {
    return x.mul(y);
  };
}

export function dSub(x) {
  return function(y) {
    return x.minus(y);
  };
}

export function dDiv(x) {
  return function(y) {
    return x.div(y);
  };
}

export function dEquals(x) {
  return function(y) {
    return x.equals(y);
  };
}

export function dCompare(x) {
  return function(y) {
    return x.cmp(y);
  };
}

export function clamp(min) {
  return function(max) {
    return function(x) {
      return x.clamp(min, max);
    };
  }
};

export function abs(x) {
  return x.abs();
}

export function pow(x) {
  return function(y) {
    return x.pow(y);
  };
}

export function exp(x) {
  return x.exp();
}

export function acos(x) {
  return x.acos();
}

export function acosh(x) {
  return x.acosh();
}

export function asin(x) {
  return x.asin();
}

export function asinh(x) {
  return x.asinh();
}

export function atan(x) {
  return x.atan();
}

export function atanh(x) {
  return x.atanh();
}

export function atan2(x) {
  return function(y) {
    return Decimal.atan2(x, y);
  };
}

export function ceil(x) {
  return x.ceil();
}

export function cos(x) {
  return x.cos();
}

export function cosh(x) {
  return x.cosh();
}

export function floor(x) {
  return x.floor();
}

export function ln(x) {
  return x.ln();
}

export function log2(x) {
  return Decimal.log2(x);
}

export function log10(x) {
  return Decimal.log10(x);
}

export function max(x) {
  return function(y) {
    return Decimal.max(x, y);
  };
}

export function min(x) {
  return function(y) {
    return Decimal.min(x, y);
  };
}

export function round(x) {
  return x.round();
}

export function truncated(x) {
  return x.truncated();
}

export function sin(x) {
  return x.sin();
}

export function sinh(x) {
  return x.sinh();
}

export function sqrt(x) {
  return x.sqrt();
}

export function tan(x) {
  return x.tan();
}

export function tanh(x) {
  return x.tanh();
}

export var e = Decimal.exp(1.0);
export var pi = new Decimal('3.14159265358979323846264338327950288419716939937510582097494459230781640628620899');

var p = [
  676.5203681218851,
  -1259.1392167224028,
  771.32342877765313,
  -176.61502916214059,
  12.507343278686905,
  -0.13857109526572012,
  9.9843695780195716e-6,
  1.5056327351493116e-7,
];

export function gamma(z) {
  var pval, i, x, y, t, zr;
  var one = new Decimal(1.0);
  if (z < 0.5) {
    // Reflection formula
    y = Decimal.div(
          pi,
          Decimal.mul(Decimal.sin(pi.mul(z)), gamma(one.sub(z))));
  }
  else {
    zr = z.sub(one);
    x = new Decimal(0.99999999999980993);
    i = 0;
    for (i = 0; i < p.length; i++) {
      pval = p[i];
      x = x.add(Decimal.div(pval, zr.add(i).add(one)));
    }
    t = zr.add(p.length).sub(0.5);
    y = Decimal.sqrt(pi.mul(2.0))
          .mul(Decimal.pow(t, zr.add(0.5)))
          .mul(Decimal.exp(t.neg()))
          .mul(x);
  }
  if (z.isInteger()) {
    return y.round();
  }
  return y;
}
