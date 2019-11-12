/*
 * @lc app=leetcode id=141 lang=java
 *
 * [141] Linked List Cycle
 */

// @lc code=start
/**
 * Definition for singly-linked list.
 * class ListNode {
 *     int val;
 *     ListNode next;
 *     ListNode(int x) {
 *         val = x;
 *         next = null;
 *     }
 * }
 */
public class Solution {
    public boolean hasCycle(ListNode head) {
        if (head == null || head.next == null) return false;
        ListNode pfast = head.next, pslow = head;

        while (pfast != null && pslow != null) {
            if (pfast == pslow) return true;

            pslow = pslow.next;
            if (pfast.next != null) pfast = pfast.next.next;
            else break;
        }

        return false;
    }
}
// 13mins
// @lc code=end

