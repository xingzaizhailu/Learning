/*
 * @lc app=leetcode id=3 lang=java
 *
 * [3] Longest Substring Without Repeating Characters
 */

// @lc code=start
class Solution {
    public int lengthOfLongestSubstring(String s) {
        HashSet set = new HashSet<Character>();
        int start = 0, ret = 0;
        
        for (int i = 0; i < s.length(); ++i) {
            char cur = s.charAt(i);
            
            if (set.contains(cur)) {
                if (i - start > ret) ret = i - start;
                
                char tmp;
                while ((tmp = s.charAt(start++)) != cur) {
                    set.remove(tmp);
                }
            } else {
                set.add(cur);
            }
        }
        
        return s.length() - start >= ret ? s.length() - start : ret;
    }
}
// @lc code=end

