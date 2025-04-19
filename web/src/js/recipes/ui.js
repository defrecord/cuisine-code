/**
 * Recipe UI component for Cuisine Code
 * Copyright (c) 2025 Aidan Pace
 */

/**
 * Creates a recipe visualization and execution UI
 */
export class RecipeUI {
  constructor(containerElement, kitchenVisualization) {
    this.container = containerElement;
    this.kitchenVisualization = kitchenVisualization;
    this.currentRecipe = null;
    this.currentStep = 0;
    this.init();
  }
  
  /**
   * Initialize the recipe UI
   */
  init() {
    this.container.innerHTML = '';
    
    // Recipe selection
    this.recipeSelector = document.createElement('select');
    this.recipeSelector.className = 'recipe-selector';
    this.recipeSelector.addEventListener('change', () => this.loadRecipe(this.recipeSelector.value));
    this.container.appendChild(this.recipeSelector);
    
    // Recipe details
    this.recipeDetails = document.createElement('div');
    this.recipeDetails.className = 'recipe-details';
    this.container.appendChild(this.recipeDetails);
    
    // Recipe steps
    this.recipeSteps = document.createElement('div');
    this.recipeSteps.className = 'recipe-steps';
    this.container.appendChild(this.recipeSteps);
    
    // Execution controls
    this.executionControls = document.createElement('div');
    this.executionControls.className = 'execution-controls';
    
    const resetButton = document.createElement('button');
    resetButton.textContent = 'Reset';
    resetButton.addEventListener('click', () => this.resetRecipe());
    this.executionControls.appendChild(resetButton);
    
    const stepButton = document.createElement('button');
    stepButton.textContent = 'Next Step';
    stepButton.addEventListener('click', () => this.executeStep());
    this.executionControls.appendChild(stepButton);
    
    const runButton = document.createElement('button');
    runButton.textContent = 'Run All';
    runButton.addEventListener('click', () => this.executeRecipe());
    this.executionControls.appendChild(runButton);
    
    this.container.appendChild(this.executionControls);
    
    // Load available recipes
    this.loadAvailableRecipes();
  }
  
  /**
   * Load available recipes from the API
   */
  async loadAvailableRecipes() {
    try {
      const response = await fetch('/api/recipes');
      const recipes = await response.json();
      
      this.recipeSelector.innerHTML = '';
      
      const defaultOption = document.createElement('option');
      defaultOption.value = '';
      defaultOption.textContent = 'Select a recipe...';
      this.recipeSelector.appendChild(defaultOption);
      
      for (const recipe of recipes) {
        const option = document.createElement('option');
        option.value = recipe.id;
        option.textContent = recipe.name;
        this.recipeSelector.appendChild(option);
      }
    } catch (error) {
      console.error('Failed to load recipes:', error);
    }
  }
  
  /**
   * Load a specific recipe
   */
  async loadRecipe(recipeId) {
    if (!recipeId) {
      this.currentRecipe = null;
      this.recipeDetails.innerHTML = '';
      this.recipeSteps.innerHTML = '';
      return;
    }
    
    try {
      const response = await fetch(`/api/recipes/${recipeId}`);
      const recipe = await response.json();
      
      this.currentRecipe = recipe;
      this.currentStep = 0;
      
      this.renderRecipeDetails();
      this.renderRecipeSteps();
    } catch (error) {
      console.error('Failed to load recipe:', error);
    }
  }
  
  /**
   * Render recipe details
   */
  renderRecipeDetails() {
    if (!this.currentRecipe) {
      this.recipeDetails.innerHTML = '';
      return;
    }
    
    this.recipeDetails.innerHTML = `
      <h2>${this.currentRecipe.name}</h2>
      <div class="recipe-metadata">
        <span class="category">${this.currentRecipe.category}</span>
        <span class="difficulty">${this.currentRecipe.difficulty}</span>
      </div>
      <p class="description">${this.currentRecipe.description}</p>
      <div class="ingredients">
        <h3>Ingredients</h3>
        <ul>
          ${this.currentRecipe.ingredients.map(ing => 
            `<li>${ing[1]} ${ing[2]} ${ing[0]}</li>`).join('')}
        </ul>
      </div>
    `;
  }
  
  /**
   * Render recipe steps
   */
  renderRecipeSteps() {
    if (!this.currentRecipe) {
      this.recipeSteps.innerHTML = '';
      return;
    }
    
    this.recipeSteps.innerHTML = '<h3>Steps</h3><ol class="steps-list"></ol>';
    const stepsList = this.recipeSteps.querySelector('.steps-list');
    
    for (let i = 0; i < this.currentRecipe.steps.length; i++) {
      const step = this.currentRecipe.steps[i];
      const stepElement = document.createElement('li');
      stepElement.className = 'step';
      if (i < this.currentStep) {
        stepElement.classList.add('completed');
      } else if (i === this.currentStep) {
        stepElement.classList.add('current');
      }
      
      stepElement.textContent = this.formatStep(step);
      stepsList.appendChild(stepElement);
    }
  }
  
  /**
   * Format a step for display
   */
  formatStep(step) {
    const operation = step[0];
    const args = step.slice(1);
    
    switch (operation) {
      case 'push':
        return `Add ${args[0]} to the stack`;
      case 'pop':
        return `Remove the top item from the stack`;
      case 'swap':
        return `Swap the top two items on the stack`;
      case 'dup':
        return `Duplicate the top item on the stack`;
      case 'transform':
        const transform = args[0];
        const params = args[1] || {};
        let paramStr = '';
        
        if (Object.keys(params).length > 0) {
          paramStr = ` (${Object.entries(params).map(([k, v]) => `${k}: ${v}`).join(', ')})`;
        }
        
        return `Apply ${transform}${paramStr} transformation`;
      default:
        return `${operation} ${args.join(' ')}`;
    }
  }
  
  /**
   * Reset the recipe execution
   */
  resetRecipe() {
    if (!this.currentRecipe) return;
    
    this.currentStep = 0;
    this.renderRecipeSteps();
    
    // Clear the kitchen stack
    while (this.kitchenVisualization.stack.length > 0) {
      this.kitchenVisualization.pop();
    }
  }
  
  /**
   * Execute a single step of the recipe
   */
  executeStep() {
    if (!this.currentRecipe || this.currentStep >= this.currentRecipe.steps.length) {
      return;
    }
    
    const step = this.currentRecipe.steps[this.currentStep];
    this.executeRecipeStep(step);
    
    this.currentStep++;
    this.renderRecipeSteps();
  }
  
  /**
   * Execute a specific recipe step
   */
  executeRecipeStep(step) {
    const operation = step[0];
    const args = step.slice(1);
    
    switch (operation) {
      case 'push':
        this.kitchenVisualization.push(args[0]);
        break;
      case 'pop':
        this.kitchenVisualization.pop();
        break;
      case 'swap':
        this.kitchenVisualization.swap();
        break;
      case 'dup':
        this.kitchenVisualization.dup();
        break;
      case 'transform':
        // In a real implementation, this would call the transformation API
        console.log(`Applying transformation: ${args[0]}`);
        break;
    }
  }
  
  /**
   * Execute the entire recipe
   */
  executeRecipe() {
    this.resetRecipe();
    
    for (let i = 0; i < this.currentRecipe.steps.length; i++) {
      const step = this.currentRecipe.steps[i];
      this.executeRecipeStep(step);
      this.currentStep++;
    }
    
    this.renderRecipeSteps();
  }
}
