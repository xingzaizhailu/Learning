/*
 * @lc app=leetcode id=142 lang=java
 *
 * [142] Linked List Cycle II
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
    public ListNode detectCycle(ListNode head) {
        if (head == null || head.next == null) return null;
        ListNode pfast = head.next, pslow = head;

        while (pfast != null && pslow != null) {
            if (pfast == pslow) return pfast;

            pslow = pslow.next;
            if (pfast.next == null) break;
            pfast = pfast.next.next;
        }

        return null;
    }
}
// 48
// @lc code=end

