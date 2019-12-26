/*
 * @lc app=leetcode id=91 lang=java
 *
 * [91] Decode Ways
 */

// @lc code=start
class Solution {
    public int numDecodings(String s) {
        if (s.length() == 0) return 0;
        return helper(s, 0);
    }
    
    private int helper(String s, int start) {
        if (start >= s.length()) return 1;
        int cur = s.charAt(start) - '0';
        if (cur == 0) return 0;
        if (start + 1 >= s.length()) return 1;
        
        int next = s.charAt(start + 1) - '0';
        if (cur <= 2 && !(cur == 2 && next > 6)) {
            return helper(s, start + 1) + helper(s, start + 2);
        }
        
        return helper(s, start + 1);
    }
}
// @lc code=end

