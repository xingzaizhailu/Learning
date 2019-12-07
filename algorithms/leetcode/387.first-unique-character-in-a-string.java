/*
 * @lc app=leetcode id=387 lang=java
 *
 * [387] First Unique Character in a String
 */

// @lc code=start
class Solution {
    public int firstUniqChar(String s) {
        int[] charSet = new int[26];
        int[] firstIdx = new int[26];
        
        for (int i = 0; i < 26; ++i) {
            charSet[i] = 0;   // 0 - how many times a char showed;
            firstIdx[i] = -1; // -1 means never showed, otherwise it's its first index
        }
        
        for (int i = 0; i < s.length(); ++i) {
            char ch = s.charAt(i);
            int chIdx = ch - 'a';
            
            charSet[chIdx] += 1;
            if (firstIdx[chIdx] < 0) firstIdx[chIdx] = i;
        }
        
        int ret = Integer.MAX_VALUE;
        for (int i = 0; i < 26; ++i) {
            if (charSet[i] == 1 && firstIdx[i] < ret) ret = firstIdx[i];
        }
        return ret == Integer.MAX_VALUE ? -1 : ret;
        
    }
}
// @lc code=end

