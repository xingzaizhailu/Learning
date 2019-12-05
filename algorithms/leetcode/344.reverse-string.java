/*
 * @lc app=leetcode id=344 lang=java
 *
 * [344] Reverse String
 */

// @lc code=start
class Solution {
    public void reverseString(char[] s) {
        int lo = 0, hi = s.length - 1;
        char ch;

        while (lo < hi) {
            ch = s[hi];
            s[hi--] = s[lo];
            s[lo++] = ch;
        }
    }
}
// @lc code=end

