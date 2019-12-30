/*
 * @lc app=leetcode id=121 lang=java
 *
 * [121] Best Time to Buy and Sell Stock
 */

// @lc code=start
class Solution {
    public int maxProfit(int[] prices) {
        if (prices.length == 0) return 0;
        int min = prices[0], ret = 0;
        
        for (int i = 1; i < prices.length; ++i) {
            if (prices[i] < min) {
                min = prices[i];   
            } else if (prices[i] - min > ret) {
                ret = prices[i] - min;
            }
        }
        
        return ret;
    }
}
// @lc code=end

