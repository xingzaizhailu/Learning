/*
 * @lc app=leetcode id=125 lang=java
 *
 * [125] Valid Palindrome
 */

// @lc code=start
class Solution {
    public boolean isPalindrome(String s) {
        int len = s.length();
        int l = 0, r = len - 1;
        while(l < r) {
            while (l < r && !isAlphanumeric(s.charAt(l))) ++l;
            while (l < r && !isAlphanumeric(s.charAt(r))) --r;
            if (l < r && Character.toLowerCase(s.charAt(l)) != Character.toLowerCase(s.charAt(r))) return false;
            ++l;
            --r;
        }
        return true;
    }

    private boolean isAlphanumeric(char ch) {
        if (Character.isLetter(ch) || Character.isDigit(ch)) return true;
        return false;
    }
}
// @lc code=end

