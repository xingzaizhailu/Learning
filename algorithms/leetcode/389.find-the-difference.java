/*
 * @lc app=leetcode id=389 lang=java
 *
 * [389] Find the Difference
 */

// @lc code=start
class Solution {
    public char findTheDifference(String s, String t) {
        HashMap<Character, Integer> hm = new HashMap<Character, Integer>();
        for (int i = 0; i < s.length(); ++i) {
            hm.put(s.charAt(i), hm.getOrDefault(s.charAt(i), 0) + 1);
        }
        
        for (int i = 0; i < t.length(); ++i) {
            int count = hm.getOrDefault(t.charAt(i), 0);
            if (count <= 0) return t.charAt(i);
            hm.put(t.charAt(i),  count - 1);
        }
        
        return 'o';
    }
}
// @lc code=end

