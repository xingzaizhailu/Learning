import java.util.Stack;

/*
 * @lc app=leetcode id=150 lang=java
 *
 * [150] Evaluate Reverse Polish Notation
 */

// @lc code=start
class Solution {
    public int evalRPN(String[] tokens) {
        Stack stack = new Stack<Integer>();

        for (String token: tokens) {
            try {
                int t = Integer.valueOf(token);
                stack.push(Integer.valueOf(token));
            } catch (NumberFormatException e) {
                int a, b;
                b = (int) stack.pop();
                a = (int) stack.pop();
                stack.push(calc(a, b, token.charAt(0)));
            }
        }

        return (int) stack.pop();
    }

    private int calc(int a, int b, char opt) {
        int ret = -1;

        if (opt == '+') ret = a + b;
        else if (opt == '-') ret = a - b;
        else if (opt == '*') ret = a * b;
        else if (opt == '/') ret = a / b;

        return ret;
    }
}
// @lc code=end

