/**
 * Kitchen visualization component for Cuisine Code
 * Copyright (c) 2025 Aidan Pace
 */

import { renderIngredient } from './ingredients.js';

/**
 * Creates a visualization of the kitchen stack
 */
export class KitchenVisualization {
  constructor(containerElement) {
    this.container = containerElement;
    this.stack = [];
    this.init();
  }
  
  /**
   * Initialize the visualization
   */
  init() {
    this.container.innerHTML = '';
    this.stackElement = document.createElement('div');
    this.stackElement.className = 'kitchen-stack';
    this.container.appendChild(this.stackElement);
    
    this.controlsElement = document.createElement('div');
    this.controlsElement.className = 'kitchen-controls';
    this.container.appendChild(this.controlsElement);
    
    this.setupControls();
    this.render();
  }
  
  /**
   * Set up kitchen controls
   */
  setupControls() {
    // Pop button
    const popButton = document.createElement('button');
    popButton.textContent = 'Pop';
    popButton.addEventListener('click', () => this.pop());
    this.controlsElement.appendChild(popButton);
    
    // Swap button
    const swapButton = document.createElement('button');
    swapButton.textContent = 'Swap';
    swapButton.addEventListener('click', () => this.swap());
    this.controlsElement.appendChild(swapButton);
    
    // Dup button
    const dupButton = document.createElement('button');
    dupButton.textContent = 'Duplicate';
    dupButton.addEventListener('click', () => this.dup());
    this.controlsElement.appendChild(dupButton);
    
    // Add ingredients section
    const ingredientsSection = document.createElement('div');
    ingredientsSection.className = 'ingredients-section';
    
    const ingredientInput = document.createElement('input');
    ingredientInput.type = 'text';
    ingredientInput.placeholder = 'Ingredient name';
    ingredientsSection.appendChild(ingredientInput);
    
    const pushButton = document.createElement('button');
    pushButton.textContent = 'Push';
    pushButton.addEventListener('click', () => {
      if (ingredientInput.value.trim()) {
        this.push(ingredientInput.value.trim());
        ingredientInput.value = '';
      }
    });
    ingredientsSection.appendChild(pushButton);
    
    this.controlsElement.appendChild(ingredientsSection);
  }
  
  /**
   * Update the stack and visualize changes
   */
  updateStack(newStack) {
    this.stack = newStack;
    this.render();
  }
  
  /**
   * Push an item onto the stack
   */
  push(item) {
    this.stack.unshift(item);
    this.render();
  }
  
  /**
   * Pop an item from the stack
   */
  pop() {
    if (this.stack.length > 0) {
      const item = this.stack.shift();
      this.render();
      return item;
    }
    return null;
  }
  
  /**
   * Swap the top two items
   */
  swap() {
    if (this.stack.length >= 2) {
      const temp = this.stack[0];
      this.stack[0] = this.stack[1];
      this.stack[1] = temp;
      this.render();
    }
  }
  
  /**
   * Duplicate the top item
   */
  dup() {
    if (this.stack.length > 0) {
      const item = this.stack[0];
      this.stack.unshift(item);
      this.render();
    }
  }
  
  /**
   * Render the stack visualization
   */
  render() {
    this.stackElement.innerHTML = '';
    
    if (this.stack.length === 0) {
      const emptyMessage = document.createElement('div');
      emptyMessage.className = 'empty-stack-message';
      emptyMessage.textContent = 'Stack is empty';
      this.stackElement.appendChild(emptyMessage);
      return;
    }
    
    for (const item of this.stack) {
      const itemElement = document.createElement('div');
      itemElement.className = 'stack-item';
      
      const visual = renderIngredient(item);
      itemElement.appendChild(visual);
      
      const label = document.createElement('div');
      label.className = 'item-label';
      label.textContent = item;
      itemElement.appendChild(label);
      
      this.stackElement.appendChild(itemElement);
    }
  }
}
