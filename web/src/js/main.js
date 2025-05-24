/**
 * Main JavaScript for Cuisine Code Website
 * Copyright (c) 2025 Aidan Pace
 */

document.addEventListener('DOMContentLoaded', () => {
  // Initialize components
  initializeNavigation();
  initializeTestimonialSlider();
  initializeAnimations();
  
  // Check if the kitchen application should be loaded
  if (document.getElementById('kitchen-app')) {
    loadKitchenApp();
  }
});

/**
 * Initialize responsive navigation
 */
function initializeNavigation() {
  const header = document.querySelector('.site-header');
  const mobileMenuToggle = document.createElement('button');
  
  mobileMenuToggle.className = 'mobile-menu-toggle';
  mobileMenuToggle.setAttribute('aria-label', 'Toggle navigation menu');
  mobileMenuToggle.innerHTML = '<span></span><span></span><span></span>';
  
  header.querySelector('.header-right').prepend(mobileMenuToggle);
  
  // Handle mobile menu toggle
  mobileMenuToggle.addEventListener('click', () => {
    header.classList.toggle('menu-open');
    
    const isOpen = header.classList.contains('menu-open');
    mobileMenuToggle.setAttribute('aria-expanded', isOpen ? 'true' : 'false');
  });
  
  // Close menu when clicking outside
  document.addEventListener('click', (event) => {
    if (header.classList.contains('menu-open') && 
        !event.target.closest('.site-header')) {
      header.classList.remove('menu-open');
      mobileMenuToggle.setAttribute('aria-expanded', 'false');
    }
  });
  
  // Handle scroll behavior
  let lastScrollTop = 0;
  
  window.addEventListener('scroll', () => {
    const scrollTop = window.pageYOffset || document.documentElement.scrollTop;
    
    // Add box shadow when scrolled
    if (scrollTop > 0) {
      header.classList.add('scrolled');
    } else {
      header.classList.remove('scrolled');
    }
    
    // Hide on scroll down, show on scroll up
    if (scrollTop > lastScrollTop && scrollTop > 100) {
      header.classList.add('hidden');
    } else {
      header.classList.remove('hidden');
    }
    
    lastScrollTop = scrollTop;
  });
}

/**
 * Initialize testimonial slider
 */
function initializeTestimonialSlider() {
  const slider = document.querySelector('.testimonial-slider');
  
  if (!slider) return;
  
  const testimonials = slider.querySelectorAll('.testimonial');
  let currentIndex = 0;
  
  // Create navigation elements
  const sliderNav = document.createElement('div');
  sliderNav.className = 'slider-nav';
  
  const prevButton = document.createElement('button');
  prevButton.className = 'slider-prev';
  prevButton.setAttribute('aria-label', 'Previous testimonial');
  prevButton.innerHTML = '&larr;';
  
  const nextButton = document.createElement('button');
  nextButton.className = 'slider-next';
  nextButton.setAttribute('aria-label', 'Next testimonial');
  nextButton.innerHTML = '&rarr;';
  
  const indicators = document.createElement('div');
  indicators.className = 'slider-indicators';
  
  testimonials.forEach((_, index) => {
    const indicator = document.createElement('button');
    indicator.className = 'slider-indicator';
    indicator.setAttribute('aria-label', `Go to testimonial ${index + 1}`);
    
    if (index === 0) {
      indicator.classList.add('active');
    }
    
    indicator.addEventListener('click', () => {
      goToSlide(index);
    });
    
    indicators.appendChild(indicator);
  });
  
  sliderNav.appendChild(prevButton);
  sliderNav.appendChild(indicators);
  sliderNav.appendChild(nextButton);
  
  slider.parentNode.appendChild(sliderNav);
  
  // Initialize slider
  testimonials.forEach((testimonial, index) => {
    testimonial.style.transform = `translateX(${(index - currentIndex) * 100}%)`;
  });
  
  // Event listeners
  prevButton.addEventListener('click', previousSlide);
  nextButton.addEventListener('click', nextSlide);
  
  // Auto-advance
  let intervalId = setInterval(nextSlide, 5000);
  
  slider.addEventListener('mouseenter', () => {
    clearInterval(intervalId);
  });
  
  slider.addEventListener('mouseleave', () => {
    intervalId = setInterval(nextSlide, 5000);
  });
  
  // Touch support
  let touchStartX = 0;
  let touchEndX = 0;
  
  slider.addEventListener('touchstart', (e) => {
    touchStartX = e.changedTouches[0].screenX;
  });
  
  slider.addEventListener('touchend', (e) => {
    touchEndX = e.changedTouches[0].screenX;
    handleSwipe();
  });
  
  function handleSwipe() {
    const swipeThreshold = 50;
    
    if (touchEndX < touchStartX - swipeThreshold) {
      nextSlide();
    } else if (touchEndX > touchStartX + swipeThreshold) {
      previousSlide();
    }
  }
  
  function goToSlide(index) {
    const indicators = document.querySelectorAll('.slider-indicator');
    
    testimonials.forEach((testimonial, i) => {
      testimonial.style.transform = `translateX(${(i - index) * 100}%)`;
      testimonial.setAttribute('aria-hidden', i !== index);
    });
    
    indicators.forEach((indicator, i) => {
      indicator.classList.toggle('active', i === index);
    });
    
    currentIndex = index;
  }
  
  function nextSlide() {
    const newIndex = (currentIndex + 1) % testimonials.length;
    goToSlide(newIndex);
  }
  
  function previousSlide() {
    const newIndex = (currentIndex - 1 + testimonials.length) % testimonials.length;
    goToSlide(newIndex);
  }
}

/**
 * Initialize scroll animations
 */
function initializeAnimations() {
  const elements = document.querySelectorAll('.feature-card, .step, .section-title');
  
  const observer = new IntersectionObserver((entries) => {
    entries.forEach(entry => {
      if (entry.isIntersecting) {
        entry.target.classList.add('visible');
        observer.unobserve(entry.target);
      }
    });
  }, {
    threshold: 0.1
  });
  
  elements.forEach(element => {
    element.classList.add('animate');
    observer.observe(element);
  });
}

/**
 * Load the kitchen application
 */
function loadKitchenApp() {
  // Dynamic import of kitchen app
  import('./kitchen/app.js')
    .then((module) => {
      const appContainer = document.getElementById('kitchen-app');
      module.initializeKitchenApp(appContainer);
    })
    .catch((error) => {
      console.error('Failed to load kitchen application:', error);
      
      const appContainer = document.getElementById('kitchen-app');
      appContainer.innerHTML = `
        <div class="error-message">
          <h3>Error Loading Application</h3>
          <p>There was a problem loading the kitchen application. Please try refreshing the page.</p>
          <button class="button button-primary" onclick="location.reload()">Refresh Page</button>
        </div>
      `;
    });
}
