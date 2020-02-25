/*
 * @lc app=leetcode id=134 lang=java
 *
 * [134] Gas Station
 */

// @lc code=start
class Solution {
    public int canCompleteCircuit(int[] gas, int[] cost) {
        int tank, ret = -1;
        
        for (int i = 0; i < gas.length; ++i) {
            if (gas[i] < cost[i]) continue;
            
            int j;
            tank = gas[i] - cost[i];
            for (j = (i + 1) % gas.length; j != i; j = ++j % gas.length) {
                tank += gas[j] - cost[j];
                if (tank < 0) break;
            }
            
            if (j == i) {
                ret = i;
                break;
            }
        }
        
        return ret;
    }
}
// @lc code=end

