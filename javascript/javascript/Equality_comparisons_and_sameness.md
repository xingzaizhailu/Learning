## Equality comparisons and sameness

JavaScript provides three different value-comparison operations:

- double equals (`==`) will perform a type conversion when comparing two things, and will handle `NaN`, `-0`, and `+0` specially to conform to IEEE 754 (so `NaN != NaN`, and `-0 == +0`);
- triple equals (`===`) will do the same comparison as double equals (including the special handling for `NaN`, `-0`, and `+0`) but without type conversion; if the types differ, `false` is returned.
- `Object.is` does no type conversion and no special handling for `NaN`, `-0`, and `+0` (giving it the same behavior as `===` except on those special numeric values).

### Sameness Comparisons

|          x          |          y          |  `==`   |  `===`  | `Object.is` | `SameValueZero` |
| :-----------------: | :-----------------: | :-----: | :-----: | :---------: | :-------------: |
|     `undefined`     |     `undefined`     | `true`  | `true`  |   `true`    |     `true`      |
|       `null`        |       `null`        | `true`  | `true`  |   `true`    |     `true`      |
|       `true`        |       `true`        | `true`  | `true`  |   `true`    |     `true`      |
|       `false`       |       `false`       | `true`  | `true`  |   `true`    |     `true`      |
|       `'foo'`       |       `'foo'`       | `true`  | `true`  |   `true`    |     `true`      |
|         `0`         |         `0`         | `true`  | `true`  |   `true`    |     `true`      |
|        `+0`         |        `-0`         | `true`  | `true`  |   `false`   |     `true`      |
|        `+0`         |         `0`         | `true`  | `true`  |   `true`    |     `true`      |
|        `-0`         |         `0`         | `true`  | `true`  |   `false`   |     `true`      |
|         `0`         |       `false`       | `true`  | `false` |   `false`   |     `false`     |
|        `""`         |       `false`       | `true`  | `false` |   `false`   |     `false`     |
|        `""`         |         `0`         | `true`  | `false` |   `false`   |     `false`     |
|        `'0'`        |         `0`         | `true`  | `false` |   `false`   |     `false`     |
|       `'17'`        |        `17`         | `true`  | `false` |   `false`   |     `false`     |
|      `[1, 2]`       |       `'1,2'`       | `true`  | `false` |   `false`   |     `false`     |
| `new String('foo')` |       `'foo'`       | `true`  | `false` |   `false`   |     `false`     |
|       `null`        |     `undefined`     | `true`  | `false` |   `false`   |     `false`     |
|       `null`        |       `false`       | `false` | `false` |   `false`   |     `false`     |
|     `undefined`     |       `false`       | `false` | `false` |   `false`   |     `false`     |
|  `{ foo: 'bar' }`   |  `{ foo: 'bar' }`   | `false` | `false` |   `false`   |     `false`     |
| `new String('foo')` | `new String('foo')` | `false` | `false` |   `false`   |     `false`     |
|         `0`         |       `null`        | `false` | `false` |   `false`   |     `false`     |
|         `0`         |        `NaN`        | `false` | `false` |   `false`   |     `false`     |
|       `'foo'`       |        `NaN`        | `false` | `false` |   `false`   |     `false`     |
|        `NaN`        |        `NaN`        | `false` | `false` |   `true`    |     `true`      |